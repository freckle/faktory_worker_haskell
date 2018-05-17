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

import Faktory.Prelude

import Data.Aeson
import Data.Aeson.Casing
import Data.ByteString.Lazy (ByteString, fromStrict)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T
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
  { clientSocket :: NS.Socket
  , clientSettings :: Settings
  }

-- | Open a new @'Client'@ connection with the given @'Settings'@
newClient :: HasCallStack => Settings -> IO Client
newClient settings@Settings{..} =
  bracketOnError (connect settingsConnection) NS.close $ \sock -> do
    let client = Client sock settings

    helloPayload <- HelloPayload
      <$> randomWorkerId
      <*> (show <$> NS.getSocketName sock)
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
  NS.close clientSocket

-- | Open a new client, yield it, then close it
withClient :: HasCallStack => Settings -> (Client -> IO ()) -> IO ()
withClient settings = bracket (newClient settings) closeClient

command :: Client -> ByteString -> [ByteString] -> IO ()
command Client{..} cmd args =  do
  let bs = BSL8.unwords (cmd:args)
  settingsLogDebug clientSettings $ "> " <> show bs
  void $ NSBL.send clientSocket (bs <> "\n")

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
      settingsLogError clientSettings err
      pure Nothing
    Right mByteString -> pure $ fromStrict <$> mByteString
 where
  readMore = NSB.recv clientSocket 4096

-- | Receive data and decode it as JSON
--
-- Parse errors result in @'Nothing'@ (at the moment).
--
recvClientJSON :: FromJSON a => Client -> IO (Maybe a)
recvClientJSON = ((decode =<<) <$>) . recvClient

-- | Connect to Host/Port
--
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
