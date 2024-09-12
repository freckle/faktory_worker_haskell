module Faktory.Settings
  ( Settings (..)
  , defaultSettings
  , envSettings

    -- * Worker
  , WorkerSettings (..)
  , defaultWorkerSettings
  , envWorkerSettings
  , Queue (..)
  , namespaceQueue
  , queueArg
  , defaultQueue
  , WorkerId
  , randomWorkerId

    -- * Pool
  , PoolSettings (..)
  , envPoolSettings

    -- * Re-exports
  , ConnectionInfo (..)
  , Namespace (..)
  ) where

import Faktory.Prelude

import Data.Aeson
import Faktory.Connection
import Faktory.JobOptions (JobOptions)
import Faktory.Settings.Queue
import Numeric.Natural
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
  , settingsOnFailed :: SomeException -> IO ()
  }

defaultWorkerSettings :: WorkerSettings
defaultWorkerSettings =
  WorkerSettings
    { settingsQueue = defaultQueue
    , settingsId = Nothing
    , settingsIdleDelay = 1
    , settingsOnFailed = \_ -> pure ()
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

instance Show WorkerId where
  show (WorkerId wid) = wid

randomWorkerId :: IO WorkerId
randomWorkerId = WorkerId . take 8 . randomRs ('a', 'z') <$> newStdGen

-- |
--
-- @since 1.1.3.0
data PoolSettings = PoolSettings
  { settingsSize :: Natural
  -- ^ Maximum pool size
  --
  -- Default is @10@. Smallest acceptable value is @1@. Note that, due to the
  -- striping behavior of @resource-pool@, a configured size @N@ may result in
  -- @N - 1@ resources.
  , settingsTimeout :: Natural
  -- ^ How long before destroying a resource, in seconds
  --
  -- Default is @600@.
  }

-- |
--
-- @since 1.1.3.0
defaultPoolSettings :: PoolSettings
defaultPoolSettings =
  PoolSettings
    { settingsSize = 10
    , settingsTimeout = 600
    }

-- | Read 'PoolSettings' from the environment
--
-- - @FAKTORY_POOL_SIZE@
-- - @FAKTORY_POOL_TIMEOUT@
--
-- @since 1.1.3.0
envPoolSettings :: IO PoolSettings
envPoolSettings =
  PoolSettings
    <$> (maybe settingsSize read <$> lookupEnv "FAKTORY_POOL_SIZE")
    <*> (maybe settingsTimeout read <$> lookupEnv "FAKTORY_POOL_TIMEOUT")
 where
  PoolSettings {..} = defaultPoolSettings
