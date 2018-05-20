module Faktory.Client
  (
  -- * Client operations
    Client
  , newClient
  , closeClient

  -- * High-level Job operations
  , pushJob
  , flush

  -- * High-level Client API
  , command_
  , commandOK
  , commandJSON
  ) where

import Faktory.Prelude

import Control.Concurrent.MVar
import Data.Aeson
import Data.Aeson.Casing
import Data.ByteString.Lazy (ByteString, fromStrict)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Faktory.Connection (connect)
import Faktory.Job
import Faktory.Protocol
import Faktory.Settings
import GHC.Generics
import GHC.Stack
import Network.Connection
import Network.Socket (HostName)
import System.Posix.Process (getProcessID)

-- | <https://github.com/contribsys/faktory/wiki/Worker-Lifecycle#initial-handshake>
data HelloPayload = HelloPayload
  { _hpWid :: Maybe WorkerId
  , _hpHostname :: HostName
  , _hpPid :: Integer -- TODO: Orphan ToJSON ProcessID
  , _hpLabels :: [String]
  , _hpV :: Int
  }
  deriving Generic

instance ToJSON HelloPayload where
   toJSON = genericToJSON $ aesonPrefix snakeCase
   toEncoding = genericToEncoding $ aesonPrefix snakeCase

data Client = Client
  { clientConnection :: MVar Connection
  , clientSettings :: Settings
  }

-- | Open a new @'Client'@ connection with the given @'Settings'@
newClient :: HasCallStack => Settings -> Maybe WorkerId -> IO Client
newClient settings@Settings{..} mWorkerId =
  bracketOnError (connect settingsConnection) connectionClose $ \conn -> do
    -- TODO: HI { "v": 2 }
    void $ recvUnsafe settings conn

    client <- Client
      <$> newMVar conn
      <*> pure settings

    helloPayload <- HelloPayload mWorkerId (show . fst $ connectionID conn)
      <$> (toInteger <$> getProcessID)
      <*> pure ["haskell"]
      <*> pure 2

    commandOK client "HELLO" [encode helloPayload]
    pure client

-- | Close a @'Client'@
closeClient :: Client -> IO ()
closeClient Client{..} = withMVar clientConnection $ \conn -> do
  sendUnsafe clientSettings conn "END" []
  connectionClose conn

-- | Push a Job to the Server
pushJob :: (HasCallStack, ToJSON arg) => Client -> Queue -> arg -> IO JobId
pushJob client queue arg = do
  job <- newJob queue arg
  commandOK client "PUSH" [encode job]
  pure $ jobJid job

-- | Clear all job data in the Faktory server
--
-- Use with caution!
--
flush :: HasCallStack => Client -> IO ()
flush client = commandOK client "FLUSH" []

-- | Send a command, read and discard the response
command_ :: Client -> ByteString -> [ByteString] -> IO ()
command_ Client{..} cmd args = withMVar clientConnection $ \conn -> do
  sendUnsafe clientSettings conn cmd args
  void $ recvUnsafe clientSettings conn

-- | Send a command, assert the response is @OK@
commandOK :: HasCallStack => Client -> ByteString -> [ByteString] -> IO ()
commandOK Client{..} cmd args = withMVar clientConnection $ \conn -> do
  sendUnsafe clientSettings conn cmd args
  response <- recvUnsafe clientSettings conn
  unless (response == Just "OK") $ throwString "Server not OK"

-- | Send a command, parse the response as JSON
commandJSON :: FromJSON a => Client -> ByteString -> [ByteString] -> IO (Maybe a)
commandJSON Client{..} cmd args = withMVar clientConnection $ \conn -> do
  sendUnsafe clientSettings conn cmd args
  mByteString <- recvUnsafe clientSettings conn
  pure $ decode =<< mByteString

-- | Send a command to the Server socket
--
-- Do not use outside of @'withMVar'@, this is not threadsafe.
--
sendUnsafe :: Settings -> Connection -> ByteString -> [ByteString] -> IO ()
sendUnsafe Settings{..} conn cmd args =  do
  let bs = BSL8.unwords (cmd:args)
  settingsLogDebug $ "> " <> show bs
  void . connectionPut conn . BSL8.toStrict $ bs <> "\n"

-- | Receive data from the Server socket
--
-- Do not use outside of @'withMVar'@, this is not threadsafe.
--
recvUnsafe :: Settings -> Connection -> IO (Maybe ByteString)
recvUnsafe Settings{..} conn = do
  eByteString <- readReply $ connectionGet conn 4096
  settingsLogDebug $ "< " <> show eByteString

  case eByteString of
    Left err -> do
      settingsLogError err
      pure Nothing
    Right mByteString -> pure $ fromStrict <$> mByteString
