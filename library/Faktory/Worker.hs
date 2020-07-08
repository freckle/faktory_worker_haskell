-- | High-level interface for a Worker
--
-- Runs forever, @FETCH@-ing Jobs from the given Queue and handing each to your
-- processing function.
--
module Faktory.Worker
  ( WorkerHalt(..)
  , runWorker
  )
where

import Faktory.Prelude

import Control.Concurrent (killThread)
import Data.Aeson
import Data.Aeson.Casing
import qualified Data.Text as T
import Faktory.Client
import Faktory.Job (Job, JobId, jobArg, jobJid)
import Faktory.Settings
import GHC.Generics
import GHC.Stack

-- | If processing functions @'throw'@ this, @'runWorker'@ will exit
data WorkerHalt = WorkerHalt
  deriving stock (Eq, Show)
  deriving anyclass Exception

newtype BeatPayload = BeatPayload
  { _bpWid :: WorkerId
  }
  deriving stock Generic

instance ToJSON BeatPayload where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

newtype AckPayload = AckPayload
  { _apJid :: JobId
  }
  deriving stock Generic

instance ToJSON AckPayload where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

data FailPayload = FailPayload
  { _fpMessage :: Text
  , _fpErrtype :: String
  , _fpJid :: JobId
  , _fpBacktrace :: [String]
  }
  deriving stock Generic

instance ToJSON FailPayload where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

runWorker
  :: FromJSON args => Settings -> WorkerSettings -> (args -> IO ()) -> IO ()
runWorker settings workerSettings f = do
  workerId <- randomWorkerId
  client <- newClient settings $ Just workerId
  beatThreadId <- forkIOWithThrowToParent $ forever $ heartBeat client workerId

  forever (processorLoop client settings workerSettings f)
    `catch` (\(_ex :: WorkerHalt) -> pure ())
    `finally` (killThread beatThreadId >> closeClient client)

processorLoop
  :: FromJSON arg
  => Client
  -> Settings
  -> WorkerSettings
  -> (arg -> IO ())
  -> IO ()
processorLoop client settings workerSettings f = do
  let
    processAndAck job = do
      f $ jobArg job
      ackJob client job

  emJob <- fetchJob client $ settingsQueue workerSettings

  case emJob of
    Left err -> settingsLogError settings $ "Invalid Job: " <> err
    Right Nothing -> threadDelaySeconds $ settingsIdleDelay workerSettings
    Right (Just job) ->
      processAndAck job
        `catches` [ Handler $ \(ex :: WorkerHalt) -> throw ex
                  , Handler $ \(ex :: SomeException) ->
                    failJob client job $ T.pack $ show ex
                  ]

-- | <https://github.com/contribsys/faktory/wiki/Worker-Lifecycle#heartbeat>
heartBeat :: Client -> WorkerId -> IO ()
heartBeat client workerId = do
  threadDelaySeconds 25
  command_ client "BEAT" [encode $ BeatPayload workerId]

fetchJob
  :: FromJSON args => Client -> Queue -> IO (Either String (Maybe (Job args)))
fetchJob client queue = commandJSON client "FETCH" [queueArg queue]

ackJob :: HasCallStack => Client -> Job args -> IO ()
ackJob client job = commandOK client "ACK" [encode $ AckPayload $ jobJid job]

failJob :: HasCallStack => Client -> Job args -> Text -> IO ()
failJob client job message =
  commandOK client "FAIL" [encode $ FailPayload message "" (jobJid job) []]
