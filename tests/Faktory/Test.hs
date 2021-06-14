module Faktory.Test
  ( module X
  , workerTestCase
  , workerTestCaseWith
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
  bracket newProducerEnv closeProducer $ \producer -> do
    void $ flush producer

  settings <- envSettings
  workerSettings <- editSettings <$> envWorkerSettings

  processedJobs <- newMVar []
  a <- async $ runWorker settings workerSettings $ \faktoryJob -> do
    let job = jobArg faktoryJob
    modifyMVar_ processedJobs $ pure . (job :)
    when (job == "BOOM") $ throw $ userError "BOOM"
    when (job == "HALT") $ throw WorkerHalt

  bracket newProducerEnv closeProducer $ \producer -> do
    run producer
    void $ perform @Text mempty producer "HALT"

  void $ wait a
  readMVar processedJobs
