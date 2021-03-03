module FaktorySpec
  ( spec
  ) where

import Faktory.Prelude

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)
import Faktory.Ent.Batch
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

  context "BATCH" $ do
    it "runs a success job if all in-batch jobs succeed" $ do
      jobs <- workerTestCase $ \producer -> do
        c <- buildJob @Text mempty producer "c"
        void $ runBatch (success c) producer $ do
          void $ batchPerform @Text mempty producer "a"
          void $ batchPerform @Text mempty producer "b"
          -- Give a little time for Faktory to fire the callback
          liftIO $ threadDelay 1000000

      jobs `shouldMatchList` ["a", "b", "c", "HALT"]

    it "does not run a success job if all jobs don't succeed" $ do
      jobs <- workerTestCase $ \producer -> do
        c <- buildJob @Text mempty producer "c"
        void $ runBatch (success c) producer $ do
          void $ batchPerform @Text mempty producer "BOOM"
          void $ batchPerform @Text mempty producer "b"
          liftIO $ threadDelay 1000000

      jobs `shouldMatchList` ["BOOM", "b", "HALT"]

    it "runs a job on complete" $ do
      jobs <- workerTestCase $ \producer -> do
        c <- buildJob @Text mempty producer "c"
        void $ runBatch (complete c) producer $ do
          void $ batchPerform @Text mempty producer "a"
          void $ batchPerform @Text mempty producer "b"
          liftIO $ threadDelay 1000000

      jobs `shouldMatchList` ["a", "b", "c", "HALT"]

    it "runs a job on complete, even if in-batch jobs fail" $ do
      jobs <- workerTestCase $ \producer -> do
        c <- buildJob @Text mempty producer "c"
        void $ runBatch (complete c) producer $ do
          void $ batchPerform @Text mempty producer "BOOM"
          void $ batchPerform @Text mempty producer "b"
          liftIO $ threadDelay 1000000

      jobs `shouldMatchList` ["BOOM", "b", "c", "HALT"]

    it "combines duplicate options in last-wins fashion" $ do
      jobs <- workerTestCase $ \producer -> do
        c <- buildJob @Text mempty producer "c"
        d <- buildJob @Text mempty producer "d"
        let options = description "foo" <> success c <> success d
        void $ runBatch options producer $ do
          void $ batchPerform @Text mempty producer "a"
          void $ batchPerform @Text mempty producer "b"
          liftIO $ threadDelay 1000000

      jobs `shouldMatchList` ["a", "b", "d", "HALT"]

    -- https://github.com/contribsys/faktory/issues/340
    xit "runs success and complete if all Jobs were successful" $ do
      jobs <- workerTestCase $ \producer -> do
        c <- buildJob @Text mempty producer "c"
        d <- buildJob @Text mempty producer "d"
        let options = description "foo" <> complete c <> success d
        void $ runBatch options producer $ do
          void $ batchPerform @Text mempty producer "a"
          void $ batchPerform @Text mempty producer "b"
          liftIO $ threadDelay 1000000

      jobs `shouldMatchList` ["a", "b", "c", "d", "HALT"]

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
    when (job == "BOOM") $ throw $ userError "BOOM"
    when (job == "HALT") $ throw WorkerHalt

  bracket newProducerEnv closeProducer $ \producer -> do
    run producer
    void $ perform @Text mempty producer "HALT"

  void $ wait a
  readMVar processedJobs
