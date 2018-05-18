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
import Faktory.Job
import Faktory.Protocol
import Faktory.Settings
import GHC.Generics
import GHC.Stack
import Network.Socket (HostName)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Network.Socket.ByteString.Lazy as NSBL
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
  { clientSocket :: MVar NS.Socket
  , clientSettings :: Settings
  }

-- | Open a new @'Client'@ connection with the given @'Settings'@
newClient :: HasCallStack => Settings -> Maybe WorkerId -> IO Client
newClient settings@Settings{..} mWorkerId =
  bracketOnError (connect settingsConnection) NS.close $ \sock -> do
    -- TODO: HI { "v": 2 }
    void $ recvUnsafe settings sock

    client <- Client
      <$> newMVar sock
      <*> pure settings

    helloPayload <- HelloPayload mWorkerId
      <$> (show <$> NS.getSocketName sock)
      <*> (toInteger <$> getProcessID)
      <*> pure ["haskell"]
      <*> pure 2

    commandOK client "HELLO" [encode helloPayload]
    pure client

-- | Close a @'Client'@
closeClient :: Client -> IO ()
closeClient Client{..} = withMVar clientSocket $ \sock -> do
  sendUnsafe clientSettings sock "END" []
  NS.close sock

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
command_ Client{..} cmd args = withMVar clientSocket $ \sock -> do
  sendUnsafe clientSettings sock cmd args
  void $ recvUnsafe clientSettings sock

-- | Send a command, assert the response is @OK@
commandOK :: HasCallStack => Client -> ByteString -> [ByteString] -> IO ()
commandOK Client{..} cmd args = withMVar clientSocket $ \sock -> do
  sendUnsafe clientSettings sock cmd args
  response <- recvUnsafe clientSettings sock
  unless (response == Just "OK") $ throwString "Server not OK"

-- | Send a command, parse the response as JSON
commandJSON :: FromJSON a => Client -> ByteString -> [ByteString] -> IO (Maybe a)
commandJSON Client{..} cmd args = withMVar clientSocket $ \sock -> do
  sendUnsafe clientSettings sock cmd args
  mByteString <- recvUnsafe clientSettings sock
  pure $ decode =<< mByteString

-- | Send a command to the Server socket
--
-- Do not use outside of @'withMVar'@, this is not threadsafe.
--
sendUnsafe :: Settings -> NS.Socket -> ByteString -> [ByteString] -> IO ()
sendUnsafe Settings{..} sock cmd args =  do
  let bs = BSL8.unwords (cmd:args)
  settingsLogDebug $ "> " <> show bs
  void $ NSBL.send sock $ bs <> "\n"

-- | Receive data from the Server socket
--
-- Do not use outside of @'withMVar'@, this is not threadsafe.
--
recvUnsafe :: Settings -> NS.Socket -> IO (Maybe ByteString)
recvUnsafe Settings{..} sock = do
  eByteString <- readReply $ NSB.recv sock 4096
  settingsLogDebug $ "< " <> show eByteString

  case eByteString of
    Left err -> do
      settingsLogError err
      pure Nothing
    Right mByteString -> pure $ fromStrict <$> mByteString

-- | Connect to Host/Port
connect :: Connection -> IO NS.Socket
connect connection =
  bracketOnError open NS.close pure
 where
  open = do
    addr <- fetchSocketAddressInfo connection
    sock <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
    NS.connect sock $ NS.addrAddress addr
    pure sock

fetchSocketAddressInfo :: Connection -> IO NS.AddrInfo
fetchSocketAddressInfo Connection{..} = do
  let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
  head <$> NS.getAddrInfo (Just hints) (Just connectionHostName) (Just $ show connectionPort)
