module Faktory.Settings
  ( Settings(..)
  , defaultSettings
  , defaultAddrInfo
  , Queue
  , queueArg
  , defaultQueue
  ) where

import Faktory.Prelude

import Data.Aeson
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.String
import Data.Text.Encoding (encodeUtf8)
import Network.Socket
  (AddrInfo(addrSocketType), SocketType(Stream), defaultHints, getAddrInfo)
import System.IO (hPutStrLn, stderr)

data Settings = Settings
  { settingsAddrInfo :: AddrInfo
  , settingsLogDebug :: String -> IO ()
  , settingsLogError :: String -> IO ()
  }

defaultSettings :: AddrInfo -> Settings
defaultSettings sockAddr = Settings
  { settingsAddrInfo = sockAddr
  , settingsLogDebug = \_msg -> pure ()
  , settingsLogError = hPutStrLn stderr . ("[ERROR]: " <>)
  }

defaultAddrInfo :: IO AddrInfo
defaultAddrInfo = do
  let hints = defaultHints { addrSocketType = Stream }
  head <$> getAddrInfo (Just hints) (Just "localhost") (Just "7419")

newtype Queue = Queue Text
  deriving newtype (IsString, FromJSON, ToJSON)

queueArg :: Queue -> ByteString
queueArg (Queue q) = fromStrict $ encodeUtf8 q

defaultQueue :: Queue
defaultQueue = "default"
