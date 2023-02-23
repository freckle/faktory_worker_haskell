-- | High-level interface for a Worker
--
-- Runs forever, @FETCH@-ing Jobs from the given Queue and handing each to your
-- processing function.
--
module Faktory.Worker (
  WorkerHalt (..),
  Worker (tid),
  jobArg,
  quietWorker,
  runWorker,
  runWorkerEnv,
  startWorker,
  waitUntilDone,
  workerId,
) where

import Faktory.Prelude
import Control.Concurrent (MVar, ThreadId, forkFinally, killThread, newEmptyMVar, putMVar, takeMVar)
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

-- | Configuration information for a faktory worker.
data WorkerConfig = WorkerConfig
  { client :: Client
  , settings :: Settings
  , wid :: WorkerId
  , workerSettings :: WorkerSettings
  }

-- | State information for a faktory worker.
data Worker = Worker
  { config :: WorkerConfig
  , isQuieted :: TVar Bool
  , isDone :: MVar (Maybe SomeException)
  , tid :: ThreadId
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

newtype WorkerM a = WorkerM
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

-- | Forks a new faktory worker and continuously polls the faktory server for
-- jobs which are passed to @'handler'@. The client is closed when the forked
-- thread ends.
startWorker
  :: (HasCallStack, FromJSON args)
  => Settings
  -> WorkerSettings
  -> (Job args -> IO ())
  -> IO Worker
startWorker settings workerSettings handler = do
  wid <- maybe randomWorkerId pure $ settingsId workerSettings
  isQuieted <- newTVarIO False
  client <- newClient settings $ Just wid
  isDone <- newEmptyMVar
  let config = WorkerConfig{client, settings, wid, workerSettings}
  tid <-
    forkFinally
      ( do
          beatThreadId <- forkIOWithThrowToParent $ forever $ heartBeat config
          finally
            ( flip runReaderT config . runWorkerM $
                catch
                  (untilM_ (liftIO $ readTVarIO isQuieted) (processorLoop handler))
                  (\(_ex :: WorkerHalt) -> pure ())
            )
            (killThread beatThreadId)
      )
      (workerCleanup client isDone)
  pure Worker{tid, config, isDone, isQuieted}
  where
    workerCleanup client isDone e =
      ( do
        closeClient client
        case e of
          Left err ->
            case fromException err of
              Just (_ :: WorkerHalt) -> pure ()
              Nothing -> putMVar isDone (Just err)
          Right () -> pure ()
        putMVar isDone Nothing
      )
        `catchAny` \cleanupEx -> do
          settingsLogError settings $ "Exception during worker cleanup: " <> displayException cleanupEx
          putMVar isDone (Just cleanupEx)

-- | Creates a new faktory worker, continuously polls the faktory server for
--- jobs which are passed to @'handler'@.
runWorker
  :: (HasCallStack, FromJSON args)
  => Settings
  -> WorkerSettings
  -> (Job args -> IO ())
  -> IO ()
runWorker settings workerSettings handler = do
  worker <- startWorker settings workerSettings handler
  void $ waitUntilDone worker

runWorkerEnv :: FromJSON args => (Job args -> IO ()) -> IO ()
runWorkerEnv f = do
  settings <- envSettings
  workerSettings <- envWorkerSettings
  runWorker settings workerSettings f

-- | Blocks until the worker thread has completed.
waitUntilDone :: Worker -> IO (Maybe SomeException)
waitUntilDone Worker{isDone} = takeMVar isDone

-- | Quiet's a worker so that it no longer polls for jobs.
quietWorker :: Worker -> IO ()
quietWorker Worker{isQuieted} = do
  atomically $ writeTVar isQuieted True

processorLoop
  :: (HasCallStack, FromJSON arg)
  => (Job arg -> IO ())
  -> WorkerM ()
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
      `withException` (\(ex :: SomeException) ->
        case fromException ex of
          Just (e :: WorkerHalt) -> throw e
          Nothing -> failJob job $ T.pack $ show ex
      )

-- | <https://github.com/contribsys/faktory/wiki/Worker-Lifecycle#heartbeat>
heartBeat :: WorkerConfig -> IO ()
heartBeat WorkerConfig{client, wid} = do
  threadDelaySeconds 25
  command_ client "BEAT" [encode $ BeatPayload wid]

fetchJob
  :: FromJSON args => Queue -> WorkerM (Either String (Maybe (Job args)))
fetchJob queue = do
  WorkerConfig{client} <- ask
  liftIO $ commandJSON client "FETCH" [queueArg queue]

ackJob :: HasCallStack => Job args -> WorkerM ()
ackJob job = do
  WorkerConfig{client} <- ask
  liftIO $ commandOK client "ACK" [encode $ AckPayload $ jobJid job]

failJob :: HasCallStack => Job args -> Text -> WorkerM ()
failJob job message = do
  WorkerConfig{client} <- ask
  liftIO $ commandOK client "FAIL" [encode $ FailPayload message "" (jobJid job) []]

workerId :: Worker -> WorkerId
workerId Worker{config = WorkerConfig{wid}} = wid
