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

import Control.Monad.IO.Class as X (MonadIO(..))
import Faktory.Job as X
import Faktory.Producer as X
import Test.Hspec as X

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Faktory.Settings
import Faktory.Worker

workerTestCase :: HasCallStack => (Producer -> IO ()) -> IO [Text]
workerTestCase = workerTestCaseWith id

workerTestCaseWith
  :: HasCallStack
  => (WorkerSettings -> WorkerSettings)
  -> (Producer -> IO ())
  -> IO [Text]
workerTestCaseWith editSettings run = do
  a <- startWorker editSettings
  withProducer run
  haltWorker a

withProducer :: (Producer -> IO a) -> IO a
withProducer f = bracket newProducerEnv closeProducer f

withWorker
  :: HasCallStack => (WorkerSettings -> WorkerSettings) -> IO a -> IO a
withWorker editSettings f = do
  a <- startWorker editSettings
  result <- f
  result <$ haltWorker a

startWorker
  :: HasCallStack => (WorkerSettings -> WorkerSettings) -> IO (Async [Text])
startWorker editSettings = do
  withProducer $ void . flush
  settings <- envSettings
  workerSettings <- editSettings <$> envWorkerSettings
  async $ do
    processedJobs <- newMVar []

    runWorker settings workerSettings $ \faktoryJob -> do
      let job = jobArg faktoryJob
      modifyMVar_ processedJobs $ pure . (job :)
      when (job == "BOOM") $ throw $ userError "BOOM"
      when (job == "HALT") $ throw WorkerHalt

    readMVar processedJobs

haltWorker :: Async a -> IO a
haltWorker a = do
  withProducer $ \producer -> void $ perform @Text mempty producer "HALT"
  wait a
