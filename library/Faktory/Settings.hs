module Faktory.Settings
  ( Settings(..)
  , ConnectionInfo(..)
  , defaultSettings
  , envSettings
  , Queue(..)
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
import Faktory.Connection
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.Random

data Settings = Settings
  { settingsQueue :: Queue
  , settingsConnection :: ConnectionInfo
  , settingsLogDebug :: String -> IO ()
  , settingsLogError :: String -> IO ()
  }

defaultSettings :: Settings
defaultSettings = Settings
  { settingsQueue = defaultQueue
  , settingsConnection = defaultConnectionInfo
  , settingsLogDebug = \_msg -> pure ()
  , settingsLogError = hPutStrLn stderr . ("[ERROR]: " <>)
  }

-- | Defaults, but read @'Connection'@ from the environment
--
-- See @'envConnection'@
--
envSettings :: IO Settings
envSettings = do
  mQueue <- lookupEnv "FAKTORY_QUEUE"
  connection <- envConnectionInfo
  pure defaultSettings
    { settingsQueue = maybe defaultQueue (Queue . pack) mQueue
    , settingsConnection = connection
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
