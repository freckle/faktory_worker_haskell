-- | High-level interface for a Worker
--
-- Runs forever, @FETCH@-ing Jobs from the given Queue and handing each to your
-- processing function.
--
module Faktory.Worker (
  WorkerHalt (..),
  WorkerConfig (..),
  runWorker,
  runWorkerEnv,
  withRunWorker,
  quietWorker,
  jobArg,
) where

import Faktory.Prelude
import Control.Concurrent (killThread)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader (ask), ReaderT (runReaderT))
import Data.Aeson
import Data.Aeson.Casing
import qualified Data.Text as T
import Faktory.Client
import Faktory.Job (Job, JobId, jobArg, jobJid, jobReserveForMicroseconds)
import Faktory.Settings
import GHC.Conc (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import GHC.Generics
import GHC.Stack
import System.Timeout (timeout)

-- | State information for a faktory worker.
data WorkerConfig = WorkerConfig
  { isQuieted :: TVar Bool
  , client :: Client
  , workerId :: WorkerId
  , workerSettings :: WorkerSettings
  , settings :: Settings
  }

-- | If processing functions @'throw'@ this, @'runWorker'@ will exit
data WorkerHalt = WorkerHalt
  deriving stock (Eq, Show)
  deriving anyclass Exception

newtype BeatPayload = BeatPayload
  { _bpWid :: WorkerId
  }
  deriving stock Generic

instance ToJSON BeatPayload where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

newtype AckPayload = AckPayload
  { _apJid :: JobId
  }
  deriving stock Generic

instance ToJSON AckPayload where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

newtype Worker a = Worker
  { runWorkerM :: ReaderT WorkerConfig IO a
  }
  deriving newtype (Functor, Applicative, Monad, MonadReader WorkerConfig, MonadIO, MonadThrow, MonadCatch, MonadMask)

data FailPayload = FailPayload
  { _fpMessage :: Text
  , _fpErrtype :: String
  , _fpJid :: JobId
  , _fpBacktrace :: [String]
  }
  deriving stock Generic

instance ToJSON FailPayload where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

untilM_ :: Monad m => m Bool -> m a -> m ()
untilM_ predicate action = do
  result <- predicate
  unless
    result
    ( do
        void action
        untilM_ predicate action
    )

-- | Creates a new faktory worker, @'action'@ is ran with @'WorkerConfig'@ before
-- polling begins. Jobs received are passed to @'handler'@. The worker's
-- connection is closed when job processing ends.
withRunWorker ::
  (HasCallStack, FromJSON args)
  =>  Settings
  -> WorkerSettings
  -> (WorkerConfig -> IO a)
  -> (Job args -> IO ())
  -> IO ()
withRunWorker settings workerSettings action handler =
  configureWorker settings workerSettings
    $ \config -> do
        void $ action config
        runWorkerWithConfig handler config

-- | Creates a new faktory worker, continuously polls the faktory server for
--- jobs which are passed to @'handler'@. The worker's connection is closed
-- when job processing ends.
runWorker
  :: (HasCallStack, FromJSON args)
  => Settings
  -> WorkerSettings
  -> (Job args -> IO ())
  -> IO ()
runWorker settings workerSettings handler =
  configureWorker settings workerSettings
    $ runWorkerWithConfig handler

-- | Creates a heartbeat thread and continuously polls jobs from the faktory
-- server. The thread is killed when the loop stops.
runWorkerWithConfig :: FromJSON arg => (Job arg -> IO ()) -> WorkerConfig -> IO ()
runWorkerWithConfig handler config = do
  beatThreadId <- forkIOWithThrowToParent $ forever $ heartBeat config
  finally
    ( flip runReaderT config . runWorkerM $
        untilM_ shouldStopWorker (processorLoop handler)
          `catch` (\(_ex :: WorkerHalt) -> pure ())
    )
    $ killThread beatThreadId

-- | Creates a new @'WorkerConfig'@ and connects to the faktory server. The
-- worker's client connection is closed after the action completes.
configureWorker
  :: HasCallStack
  => Settings
  -> WorkerSettings
  -> (WorkerConfig -> IO a)
  -> IO a
configureWorker settings workerSettings =
  bracket
    ( do
        workerId <- maybe randomWorkerId pure $ settingsId workerSettings
        isQuieted <- newTVarIO False
        client <- newClient settings $ Just workerId
        pure $ WorkerConfig{isQuieted, workerId, client, workerSettings, settings}
    )
    (\WorkerConfig{client} -> closeClient client)

runWorkerEnv :: FromJSON args => (Job args -> IO ()) -> IO ()
runWorkerEnv f = do
  settings <- envSettings
  workerSettings <- envWorkerSettings
  runWorker settings workerSettings f

-- | Quiet's a worker so that it no longer polls for jobs.
quietWorker :: WorkerConfig -> IO ()
quietWorker WorkerConfig{isQuieted} = do
  atomically $ writeTVar isQuieted True

shouldStopWorker :: Worker Bool
shouldStopWorker = do
  WorkerConfig{isQuieted} <- ask
  liftIO $ readTVarIO isQuieted

processorLoop
  :: (HasCallStack, FromJSON arg)
  => (Job arg -> IO ())
  -> Worker ()
processorLoop f = do
  WorkerConfig{settings, workerSettings} <- ask
  let
    namespace = connectionInfoNamespace $ settingsConnection settings
    processAndAck job = do
      mResult <- liftIO $ timeout (jobReserveForMicroseconds job) $ f job
      case mResult of
        Nothing -> liftIO $ settingsLogError settings "Job reservation period expired."
        Just () -> ackJob job

  emJob <- fetchJob $ namespaceQueue namespace $ settingsQueue
    workerSettings

  case emJob of
    Left err -> liftIO $ settingsLogError settings $ "Invalid Job: " <> err
    Right Nothing -> liftIO $ threadDelaySeconds $ settingsIdleDelay workerSettings
    Right (Just job) ->
      processAndAck job
        `catches` [ Handler $ \(ex :: WorkerHalt) -> throw ex
                  , Handler $ \(ex :: SomeException) ->
                    failJob job $ T.pack $ show ex
                  ]

-- | <https://github.com/contribsys/faktory/wiki/Worker-Lifecycle#heartbeat>
heartBeat :: WorkerConfig -> IO ()
heartBeat WorkerConfig{client, workerId} = do
  threadDelaySeconds 25
  command_ client "BEAT" [encode $ BeatPayload workerId]

fetchJob
  :: FromJSON args => Queue -> Worker (Either String (Maybe (Job args)))
fetchJob queue = do
  WorkerConfig{client} <- ask
  liftIO $ commandJSON client "FETCH" [queueArg queue]

ackJob :: HasCallStack => Job args -> Worker ()
ackJob job = do
  WorkerConfig{client} <- ask
  liftIO $ commandOK client "ACK" [encode $ AckPayload $ jobJid job]

failJob :: HasCallStack => Job args -> Text -> Worker ()
failJob job message = do
  WorkerConfig{client} <- ask
  liftIO $ commandOK client "FAIL" [encode $ FailPayload message "" (jobJid job) []]
