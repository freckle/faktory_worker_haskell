module Faktory.Settings
  ( Settings(..)
  , defaultSettings
  , envSettings
  , WorkerSettings(..)
  , defaultWorkerSettings
  , envWorkerSettings
  , Queue(..)
  , namespaceQueue
  , queueArg
  , defaultQueue
  , WorkerId
  , randomWorkerId

  -- * Re-exports
  , ConnectionInfo(..)
  , Namespace(..)
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
  { settingsConnection :: ConnectionInfo
  , settingsLogDebug :: String -> IO ()
  , settingsLogError :: String -> IO ()
  }

defaultSettings :: Settings
defaultSettings = Settings
  { settingsConnection = defaultConnectionInfo
  , settingsLogDebug = \_msg -> pure ()
  , settingsLogError = hPutStrLn stderr . ("[ERROR]: " <>)
  }

-- | Defaults, but read @'Connection'@ from the environment
--
-- See @'envConnection'@
--
envSettings :: IO Settings
envSettings = do
  connection <- envConnectionInfo
  pure defaultSettings { settingsConnection = connection }

data WorkerSettings = WorkerSettings
  { settingsQueue :: Queue
  , settingsId :: Maybe WorkerId
  , settingsIdleDelay :: Int
  }

defaultWorkerSettings :: WorkerSettings
defaultWorkerSettings = WorkerSettings
  { settingsQueue = defaultQueue
  , settingsId = Nothing
  , settingsIdleDelay = 1
  }

envWorkerSettings :: IO WorkerSettings
envWorkerSettings = do
  mQueue <- lookupEnv "FAKTORY_QUEUE"
  mWorkerId <- lookupEnv "FAKTORY_WORKER_ID"
  pure defaultWorkerSettings
    { settingsQueue = maybe defaultQueue (Queue . pack) mQueue
    , settingsId = WorkerId <$> mWorkerId
    }

newtype Queue = Queue Text
  deriving newtype (IsString, FromJSON, ToJSON)

namespaceQueue :: Namespace -> Queue -> Queue
namespaceQueue (Namespace n) (Queue q) = Queue $ mappend n q

queueArg :: Queue -> ByteString
queueArg (Queue q) = fromStrict $ encodeUtf8 q

defaultQueue :: Queue
defaultQueue = "default"

newtype WorkerId = WorkerId String
  deriving newtype (FromJSON, ToJSON)

randomWorkerId :: IO WorkerId
randomWorkerId = WorkerId . take 8 . randomRs ('a', 'z') <$> newStdGen
