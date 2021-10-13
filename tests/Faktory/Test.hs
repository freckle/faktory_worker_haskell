module Faktory.Test
  ( module X
  , workerTestCase
  , workerTestCaseWith
  , TestJob(..)

  -- * Lower-level
  , withProducer
  , withWorker
  , startWorker
  , haltWorker
  )
where

import Faktory.Prelude as X

import Control.Monad.IO.Class as X (MonadIO(..))
import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString(..))
import Faktory.Job as X
import Faktory.Producer as X
import GHC.Generics (Generic)
import Test.Hspec as X

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Faktory.Settings
import Faktory.Worker

workerTestCase :: HasCallStack => (Producer -> IO ()) -> IO [TestJob]
workerTestCase = workerTestCaseWith id

workerTestCaseWith
  :: HasCallStack
  => (WorkerSettings -> WorkerSettings)
  -> (Producer -> IO ())
  -> IO [TestJob]
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
  :: HasCallStack => (WorkerSettings -> WorkerSettings) -> IO (Async [TestJob])
startWorker editSettings = do
  withProducer $ void . flush
  settings <- envSettings
  workerSettings <- editSettings <$> envWorkerSettings
  async $ do
    processedJobs <- newMVar []

    runWorker settings workerSettings $ \faktoryJob -> do
      let job = jobArg faktoryJob
      when (job == Wait) $ threadDelay 3000000
      modifyMVar_ processedJobs $ pure . (job :)
      when (job == Boom) $ throw $ userError "BOOM"
      when (job == Halt) $ throw WorkerHalt

    readMVar processedJobs

haltWorker :: Async a -> IO a
haltWorker a = do
  withProducer $ \producer -> void $ perform mempty producer Halt
  wait a

data TestJob = Wait | Boom | Halt | J Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance IsString TestJob where
  fromString = J . pack

instance HasJobType TestJob where
  jobTypeName _ = "TestJob"
