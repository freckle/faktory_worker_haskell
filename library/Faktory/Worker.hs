-- | High-level interface for a Worker
--
-- Runs forever, @FETCH@-ing Jobs from the given Queue and handing each to your
-- processing function.
--
module Faktory.Worker
  ( WorkerHalt(..)
  , runWorker
  ) where

import Faktory.Prelude

import Control.Concurrent
import Data.Aeson
import Data.Aeson.Casing
import qualified Data.Text as T
import Faktory.Client
import Faktory.Job
import Faktory.Settings
import GHC.Generics
import GHC.Stack

-- | If processing functions @'throw'@ this, @'runWorker'@ will exit
data WorkerHalt = WorkerHalt
  deriving (Eq, Show, Exception)

newtype BeatPayload = BeatPayload
  { _bpWid :: WorkerId
  }
  deriving Generic

instance ToJSON BeatPayload where
   toJSON = genericToJSON $ aesonPrefix snakeCase
   toEncoding = genericToEncoding $ aesonPrefix snakeCase

newtype AckPayload = AckPayload
  { _apJid :: JobId
  }
  deriving Generic

instance ToJSON AckPayload where
   toJSON = genericToJSON $ aesonPrefix snakeCase
   toEncoding = genericToEncoding $ aesonPrefix snakeCase

data FailPayload = FailPayload
  { _fpMessage :: Text
  , _fpErrtype :: String
  , _fpJid :: JobId
  , _fpBacktrace :: [String]
  }
  deriving Generic

instance ToJSON FailPayload where
   toJSON = genericToJSON $ aesonPrefix snakeCase
   toEncoding = genericToEncoding $ aesonPrefix snakeCase

runWorker :: FromJSON args => Settings -> Queue -> (args -> IO ()) -> IO ()
runWorker settings queue f = do
  workerId <- randomWorkerId
  client <- newClient settings $ Just workerId
  beatThreadId <- forkIO $ forever $ heartBeat client workerId

  forever (processorLoop client queue f)
    `catch` (\(_ex :: WorkerHalt) -> pure ())
    `finally` (killThread beatThreadId >> closeClient client)

processorLoop :: FromJSON arg => Client -> Queue -> (arg -> IO ()) -> IO ()
processorLoop client queue f = do
  let
    processAndAck job = do
      f $ jobArg job
      ackJob client job

  mJob <- fetchJob client queue

  for_ mJob $ \job ->
    processAndAck job `catches`
      [ Handler $ \(ex :: WorkerHalt) -> throw ex
      , Handler $ \(ex :: SomeException) -> failJob client job $ T.pack $ show ex
      ]

-- | <https://github.com/contribsys/faktory/wiki/Worker-Lifecycle#heartbeat>
heartBeat :: Client -> WorkerId -> IO ()
heartBeat client workerId = do
  threadDelay $ 25 * 1000000
  command_ client "BEAT" [encode $ BeatPayload workerId]

fetchJob :: FromJSON args => Client -> Queue -> IO (Maybe (Job args))
fetchJob client queue =
  -- Treat an exception here like no Job. The Client will have already logged
  -- the protocol error to stderr, and we should just keep chugging.
  handleAny (const $ pure Nothing)
    $ commandJSON client "FETCH" [queueArg queue]

ackJob :: HasCallStack => Client -> Job args -> IO ()
ackJob client job =
  commandOK client "ACK" [encode $ AckPayload $ jobJid job]

failJob :: HasCallStack => Client -> Job args -> Text -> IO ()
failJob client job message =
  commandOK client "FAIL" [encode $ FailPayload message "" (jobJid job) []]
