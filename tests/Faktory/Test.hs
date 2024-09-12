module Faktory.Test
  ( module X
  , workerTestCase
  , workerTestCaseWith

    -- * Lower-level
  , withProducer
  , withWorker
  , startWorker
  , haltWorker
  )
where

import Faktory.Prelude as X

import Control.Monad.IO.Class as X (MonadIO (..))
import Faktory.Job as X
import Faktory.Producer as X
import Test.Hspec as X

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Faktory.Settings
import Faktory.Worker

workerTestCase :: HasCallStack => (Producer -> IO ()) -> IO [Job Text]
workerTestCase = workerTestCaseWith id

workerTestCaseWith
  :: HasCallStack
  => (WorkerSettings -> WorkerSettings)
  -> (Producer -> IO ())
  -> IO [Job Text]
workerTestCaseWith editSettings =
  fmap (\(x, _, _) -> x) . withWorker editSettings . withProducer

withProducer :: (Producer -> IO a) -> IO a
withProducer f = bracket newProducerEnv closeProducer f

withWorker
  :: HasCallStack
  => (WorkerSettings -> WorkerSettings)
  -> IO a
  -> IO ([Job Text], [SomeException], a)
withWorker editSettings f = do
  a <- startWorker editSettings
  result <- f
  (processed, failed) <- haltWorker a
  pure (processed, failed, result)

startWorker
  :: HasCallStack
  => (WorkerSettings -> WorkerSettings)
  -> IO (Async ([Job Text], [SomeException]))
startWorker editSettings = do
  withProducer $ void . flush
  settings <- envSettings
  workerSettings <- editSettings <$> envWorkerSettings
  async $ do
    processedJobs <- newMVar []
    failedJobs <- newMVar []

    let workerSettings' =
          workerSettings
            { settingsOnFailed = \ex ->
                modifyMVar_ failedJobs $ pure . (ex :)
            }

    runWorker settings workerSettings' $ \faktoryJob -> do
      let job = jobArg faktoryJob
      when (job == "WAIT") $ threadDelay 3000000
      modifyMVar_ processedJobs $ pure . (faktoryJob :)
      when (job == "BOOM") $ throw $ userError "BOOM"
      when (job == "HALT") $ throw WorkerHalt

    (,)
      <$> readMVar processedJobs
      <*> readMVar failedJobs

haltWorker :: Async a -> IO a
haltWorker a = do
  withProducer $ \producer -> void $ perform @Text mempty producer "HALT"
  wait a
