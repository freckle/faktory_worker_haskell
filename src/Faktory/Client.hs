{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Faktory.Client
  (
  -- * Client operations
    Client
  , newClient
  , closeClient
  , withClient

  -- * High-level Job operations
  , pushJob
  , withJob

  -- * Low-level Job operations
  , fetchJob
  , ackJob
  , failJob

  -- * High-level Client API
  , command
  , assertOK
  ) where

import Control.Exception.Safe
import Control.Monad (unless, void, when)
import Data.Aeson
import Data.Aeson.Casing
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString, fromStrict)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Foldable (for_)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Faktory.Job
import Faktory.Protocol
import Faktory.Settings
import GHC.Generics
import GHC.Stack
import Network
import qualified Network.BSD as BSD
import Network.HostName (getHostName)
import qualified Network.Socket as NS
import System.IO
import System.Posix.Process (getProcessID)
import System.Random

data HelloPayload = HelloPayload
  { _hpWid :: String
  , _hpHostname :: HostName
  , _hpPid :: Integer -- TODO: Orphan ToJSON ProcessID
  , _hpLabels :: [String]
  , _hpV :: Int
  }
  deriving Generic

instance ToJSON HelloPayload where
   toJSON = genericToJSON $ aesonPrefix snakeCase
   toEncoding = genericToEncoding $ aesonPrefix snakeCase

-- Ruby uses 8 random hex
randomWorkerId :: IO String
randomWorkerId = take 8 . randomRs ('a', 'z') <$> newStdGen

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

data Client = Client
  { clientHandle :: Handle
  , clientSettings :: Settings
  }

-- | Open a new @'Client'@ connection with the given @'Settings'@
newClient :: HasCallStack => Settings -> IO Client
newClient settings@Settings{..} = do
  h <- connect settingsHost settingsPort

  let client = Client h settings

  helloPayload <- HelloPayload
    <$> randomWorkerId
    <*> getHostName
    <*> (toInteger <$> getProcessID)
    <*> pure ["haskell"]
    <*> pure 2

  -- TODO: HI { "v": 2 }
  void $ recvClient client
  command client "HELLO" [encode helloPayload]
  assertOK client

  pure client

-- | Close a @'Client'@
--
-- N.B. nothing prevents you from accidentally using a closed client. The
-- behavior when that happens is undefined. It's recommended you use
-- @'withClient'@ and never close a @'Client'@ yourself.
--
closeClient :: Client -> IO ()
closeClient client@Client{..} = do
  command client "END" []
  disconnect clientHandle

-- | Open a new client, yield it, then close it
withClient :: HasCallStack => Settings -> (Client -> IO ()) -> IO ()
withClient settings = bracket (newClient settings) closeClient

command :: Client -> ByteString -> [ByteString] -> IO ()
command Client{..} cmd args =  do
  let bs = BSL8.unwords (cmd:args)
  settingsLogDebug clientSettings $ "> " <> show bs
  BSL8.hPutStrLn clientHandle bs

assertOK :: HasCallStack => Client -> IO ()
assertOK client = do
  let expected = Just "OK"
  actual <- recvClient client
  unless (actual == expected) $ throwString "Server not OK"

pushJob :: (HasCallStack, ToJSON arg) => Client -> Queue -> arg -> IO JobId
pushJob client queue arg = do
  job <- newJob queue arg
  command client "PUSH" [encode job]
  assertOK client
  pure $ jobJid job

withJob :: FromJSON arg => Client -> Queue -> (arg -> IO ()) -> IO ()
withJob client queue f = do
  mJob <- fetchJob client queue

  for_ mJob $ \job ->
    handleAny (failJob client job . T.pack . show) $ do
      f $ jobArg job
      ackJob client job

fetchJob :: FromJSON args => Client -> Queue -> IO (Maybe (Job args))
fetchJob client queue = do
  command client "FETCH" [queueArg queue]
  recvClientJSON client

ackJob :: HasCallStack => Client -> Job args -> IO ()
ackJob client job = do
  command client "ACK" [encode $ AckPayload $ jobJid job]
  assertOK client

failJob :: HasCallStack => Client -> Job args -> Text -> IO ()
failJob client job message = do
  command client "FAIL" [encode $ FailPayload message "" (jobJid job) []]
  assertOK client

recvClient :: Client -> IO (Maybe ByteString)
recvClient Client{..} = do
  eByteString <- readReply readMore
  settingsLogDebug clientSettings $ "< " <> show eByteString

  case eByteString of
    Left err -> do
      settingsLogError clientSettings $ "Error: " <> err
      pure Nothing
    Right mByteString -> pure $ fromStrict <$> mByteString
 where
  readMore = do
    hFlush clientHandle
    BS.hGetSome clientHandle 4096

-- | Receive data and decode it as JSON
--
-- Parse errors result in @'Nothing'@ (at the moment).
--
recvClientJSON :: FromJSON a => Client -> IO (Maybe a)
recvClientJSON = ((decode =<<) <$>) . recvClient

-- | Connect to Host/Port
--
-- Taken from hedis.
--
connect :: HostName -> PortID -> IO Handle
connect hostName (PortNumber port) =
  bracketOnError hConnect hClose $ \connHandle -> do
    hSetBinaryMode connHandle True
    pure connHandle
 where
  hConnect =
    bracketOnError mkSocket NS.close $ \sock -> do
      NS.setSocketOption sock NS.KeepAlive 1
      host <- BSD.getHostByName hostName
      NS.connect sock $ NS.SockAddrInet port $ BSD.hostAddress host
      NS.socketToHandle sock ReadWriteMode
  mkSocket = NS.socket NS.AF_INET NS.Stream 0

connect hostName portId = connectTo hostName portId

-- | Disconnect and close the Handle
--
-- Taken from hedis.
--
disconnect :: Handle -> IO ()
disconnect connHandle = do
  open <- hIsOpen connHandle
  when open (hClose connHandle)
