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
import Crypto.Hash (Digest, SHA256(..), hashWith)
import Data.Aeson
import Data.ByteArray (ByteArrayAccess)
import Data.ByteString.Lazy (ByteString, fromStrict)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Faktory.Connection (connect)
import Faktory.Protocol
import Faktory.Settings
import GHC.Stack
import Network.Connection
import Network.Socket (HostName)
import System.Posix.Process (getProcessID)

data Client = Client
  { clientConnection :: MVar Connection
  , clientSettings :: Settings
  }

-- | <https://github.com/contribsys/faktory/wiki/Worker-Lifecycle#initial-handshake>
data HiPayload = HiPayload
  { hiVersion :: Int
  , hiNonce :: Maybe Text
  , hiIterations :: Maybe Int
  }

instance FromJSON HiPayload where
  parseJSON = withObject "HiPayload"
    $ \o -> HiPayload <$> o .: "v" <*> o .:? "s" <*> o .:? "i"

data HelloPayload = HelloPayload
  { helloWorkerId :: Maybe WorkerId
  , helloHostname :: HostName
  , helloProcessId :: Integer -- TODO: Orphan ToJSON ProcessID
  , helloLabels :: [Text]
  , helloVersion :: Int
  , helloPasswordHash :: Maybe Text
  }

instance ToJSON HelloPayload where
  toJSON HelloPayload {..} = object
    [ "wid" .= helloWorkerId
    , "hostname" .= helloHostname
    , "pid" .= helloProcessId
    , "labels" .= helloLabels
    , "v" .= helloVersion
    , "pwdhash" .= helloPasswordHash
    ]
  toEncoding HelloPayload {..} = pairs $ mconcat
    [ "wid" .= helloWorkerId
    , "hostname" .= helloHostname
    , "pid" .= helloProcessId
    , "labels" .= helloLabels
    , "v" .= helloVersion
    , "pwdhash" .= helloPasswordHash
    ]

-- | Open a new @'Client'@ connection with the given @'Settings'@
newClient :: HasCallStack => Settings -> Maybe WorkerId -> IO Client
newClient settings@Settings {..} mWorkerId =
  bracketOnError (connect settingsConnection) connectionClose $ \conn -> do
    client <- Client <$> newMVar conn <*> pure settings

    greeting <-
      fromJustThrows "Unexpected end of HI message"
      =<< fromRightThrows
      =<< recvUnsafe settings conn
    stripped <-
      fromJustThrows ("Missing HI prefix: " <> show greeting)
        $ BSL8.stripPrefix "HI" greeting
    HiPayload {..} <-
      fromJustThrows ("Failed to parse HI payload: " <> show stripped)
        $ decode stripped

    when (hiVersion > expectedProtocolVersion) $ settingsLogError $ concat
      [ "Server's protocol version "
      , show hiVersion
      , " higher than client's expected protocol version "
      , show expectedProtocolVersion
      ]

    let
      mPassword = connectionInfoPassword settingsConnection
      mHashedPassword = hashPassword <$> hiNonce <*> hiIterations <*> mPassword

    helloPayload <-
      HelloPayload mWorkerId (show . fst $ connectionID conn)
      <$> (toInteger <$> getProcessID)
      <*> pure ["haskell"]
      <*> pure expectedProtocolVersion
      <*> pure mHashedPassword

    commandOK client "HELLO" [encode helloPayload]
    pure client
 where
  fromJustThrows message = maybe (throwString message) pure
  fromRightThrows = either throwString pure

-- | Close a @'Client'@
closeClient :: Client -> IO ()
closeClient Client {..} = withMVar clientConnection $ \conn -> do
  sendUnsafe clientSettings conn "END" []
  connectionClose conn

-- | Push a Job to the Server
pushJob :: (HasCallStack, ToJSON a) => Client -> a -> IO ()
pushJob client job = commandOK client "PUSH" [encode job]

-- | Clear all job data in the Faktory server
--
-- Use with caution!
--
flush :: HasCallStack => Client -> IO ()
flush client = commandOK client "FLUSH" []

-- | Send a command, read and discard the response
command_ :: Client -> ByteString -> [ByteString] -> IO ()
command_ Client {..} cmd args = withMVar clientConnection $ \conn -> do
  sendUnsafe clientSettings conn cmd args
  void $ recvUnsafe clientSettings conn

-- | Send a command, assert the response is @OK@
commandOK :: HasCallStack => Client -> ByteString -> [ByteString] -> IO ()
commandOK Client {..} cmd args = withMVar clientConnection $ \conn -> do
  sendUnsafe clientSettings conn cmd args
  response <- recvUnsafe clientSettings conn
  unless (response == Right (Just "OK"))
    $ throwString
    $ "Server not OK. Reply was: "
    <> show response

-- | Send a command, parse the response as JSON
commandJSON
  :: FromJSON a
  => Client
  -> ByteString
  -> [ByteString]
  -> IO (Either String (Maybe a))
commandJSON Client {..} cmd args = withMVar clientConnection $ \conn -> do
  sendUnsafe clientSettings conn cmd args
  emByteString <- recvUnsafe clientSettings conn

  case emByteString of
    Left err -> pure $ Left err
    Right mByteString -> pure $ traverse eitherDecode mByteString

-- | Send a command to the Server socket
--
-- Do not use outside of @'withMVar'@, this is not threadsafe.
--
sendUnsafe :: Settings -> Connection -> ByteString -> [ByteString] -> IO ()
sendUnsafe Settings {..} conn cmd args = do
  let bs = BSL8.unwords (cmd : args)
  settingsLogDebug $ "> " <> show bs
  void . connectionPut conn . BSL8.toStrict $ bs <> "\n"

-- | Receive data from the Server socket
--
-- Do not use outside of @'withMVar'@, this is not threadsafe.
--
recvUnsafe :: Settings -> Connection -> IO (Either String (Maybe ByteString))
recvUnsafe Settings {..} conn = do
  eByteString <- readReply $ connectionGet conn 4096
  settingsLogDebug $ "< " <> show eByteString

  case eByteString of
    Left err -> pure $ Left err
    Right mByteString -> pure . Right $ fromStrict <$> mByteString

-- | Iteratively apply a function @n@ times
--
-- This is like @iterate f s !! n@ but strict in @s@
--
times :: Int -> (s -> s) -> s -> s
times n f !s
  | n <= 0 = s
  | otherwise = times (n - 1) f (f s)

-- | Hash password using provided @nonce@ for @n@ iterations
hashPassword :: Text -> Int -> String -> Text
hashPassword nonce n password =
  T.pack
    . show
    . times (n - 1) hash
    . hash
    . T.encodeUtf8
    $ T.pack password
    <> nonce
 where
  -- Note that we use hash at two different types above.
  --
  -- 1. hash :: ByteString    -> Digest SHA256
  -- 2. hash :: Digest SHA256 -> Digest SHA256
  hash :: (ByteArrayAccess b) => b -> Digest SHA256
  hash = hashWith SHA256

-- | Protocol version the client expects
expectedProtocolVersion :: Int
expectedProtocolVersion = 2
