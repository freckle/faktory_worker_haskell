module FaktorySpec
  ( spec
  ) where

import Faktory.Prelude

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Faktory.Job
import Faktory.Producer
import Faktory.Settings
import Faktory.Worker
import Test.Hspec

spec :: Spec
spec = describe "Faktory" $ do
  it "can push and process jobs" $ do
    jobs <- workerTestCase $ \producer -> do
      void $ perform @Text mempty producer "a"
      void $ perform @Text mempty producer "b"

    jobs `shouldMatchList` ["a", "b", "HALT"]

  it "can push jobs with optional attributes" $ do
    jobs <- workerTestCase $ \producer -> do
      void $ perform @Text once producer "a"
      void $ perform @Text (retry 0) producer "b"

    jobs `shouldMatchList` ["a", "b", "HALT"]

  it "correctly handles fetch timeouts" $ do
    -- Pause longer than the fetch timeout
    --
    -- https://github.com/contribsys/faktory/wiki/Worker-Lifecycle#fetching-jobs
    --
    -- This ensures that the worker loop experiences recieving a Nothing from
    -- the Server and handles it correctly. Setting our own idle delay to 0
    -- ensures that we'll pick up the following HALT message immediately.
    --
    let editSettings ws = ws { settingsIdleDelay = 0 }
    jobs <- workerTestCaseWith editSettings $ \_ -> do
      threadDelay $ 2 * 1000000 + 250000

    jobs `shouldMatchList` ["HALT"]

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
  a <- async $ runWorker settings workerSettings $ \job -> do
    modifyMVar_ processedJobs $ pure . (job :)
    when (job == "HALT") $ throw WorkerHalt

  bracket newProducerEnv closeProducer $ \producer -> do
    run producer
    void $ perform @Text mempty producer "HALT"

  void $ wait a
  readMVar processedJobs
