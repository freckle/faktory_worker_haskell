module Faktory.Settings
  ( Settings(..)
  , Connection(..)
  , defaultSettings
  , Queue
  , queueArg
  , defaultQueue
  , WorkerId
  , randomWorkerId
  ) where

import Faktory.Prelude

import Data.Aeson
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.String
import Data.Text.Encoding (encodeUtf8)
import Network.Socket (HostName, PortNumber)
import System.IO (hPutStrLn, stderr)
import System.Random

data Connection = Connection
  { connectionHostName :: HostName
  , connectionPort :: PortNumber
  }

data Settings = Settings
  { settingsConnection :: Connection
  , settingsLogDebug :: String -> IO ()
  , settingsLogError :: String -> IO ()
  }

defaultSettings :: Settings
defaultSettings = Settings
  { settingsConnection = Connection
    { connectionHostName = "localhost"
    , connectionPort = 7419
    }
  , settingsLogDebug = \_msg -> pure ()
  , settingsLogError = hPutStrLn stderr . ("[ERROR]: " <>)
  }

newtype Queue = Queue Text
  deriving newtype (IsString, FromJSON, ToJSON)

queueArg :: Queue -> ByteString
queueArg (Queue q) = fromStrict $ encodeUtf8 q

defaultQueue :: Queue
defaultQueue = "default"

newtype WorkerId = WorkerId String
  deriving newtype (FromJSON, ToJSON)

randomWorkerId :: IO WorkerId
randomWorkerId = WorkerId . take 8 . randomRs ('a', 'z') <$> newStdGen
