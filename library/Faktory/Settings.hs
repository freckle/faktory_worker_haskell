module Faktory.Settings
  ( Settings (..)
  , defaultSettings
  , envSettings
  , WorkerSettings (..)
  , defaultWorkerSettings
  , envWorkerSettings
  , Queue (..)
  , namespaceQueue
  , queueArg
  , defaultQueue
  , WorkerId
  , randomWorkerId

    -- * Re-exports
  , ConnectionInfo (..)
  , Namespace (..)
  ) where

import Faktory.Prelude

import Data.Aeson
import Faktory.Connection
import Faktory.JobOptions (JobOptions)
import Faktory.Settings.Queue
import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import System.Random

data Settings = Settings
  { settingsConnection :: ConnectionInfo
  , settingsLogDebug :: String -> IO ()
  , settingsLogError :: String -> IO ()
  , settingsDefaultJobOptions :: JobOptions
  }

defaultSettings :: Settings
defaultSettings =
  Settings
    { settingsConnection = defaultConnectionInfo
    , settingsLogDebug = \_msg -> pure ()
    , settingsLogError = hPutStrLn stderr . ("[ERROR]: " <>)
    , settingsDefaultJobOptions = mempty
    }

-- | Defaults, but read @'Connection'@ from the environment
--
-- See @'envConnection'@
envSettings :: IO Settings
envSettings = do
  connection <- envConnectionInfo
  pure defaultSettings {settingsConnection = connection}

data WorkerSettings = WorkerSettings
  { settingsQueue :: Queue
  , settingsId :: Maybe WorkerId
  , settingsIdleDelay :: Int
  }

defaultWorkerSettings :: WorkerSettings
defaultWorkerSettings =
  WorkerSettings
    { settingsQueue = defaultQueue
    , settingsId = Nothing
    , settingsIdleDelay = 1
    }

envWorkerSettings :: IO WorkerSettings
envWorkerSettings = do
  mQueue <- lookupEnv "FAKTORY_QUEUE"
  mWorkerId <- lookupEnv "FAKTORY_WORKER_ID"
  pure
    defaultWorkerSettings
      { settingsQueue = maybe defaultQueue (Queue . pack) mQueue
      , settingsId = WorkerId <$> mWorkerId
      }

newtype WorkerId = WorkerId String
  deriving newtype (FromJSON, ToJSON)

randomWorkerId :: IO WorkerId
randomWorkerId = WorkerId . take 8 . randomRs ('a', 'z') <$> newStdGen
