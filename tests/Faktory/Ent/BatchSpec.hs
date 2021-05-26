module Faktory.Ent.BatchSpec
  ( spec
  ) where

import Faktory.Test

import Control.Concurrent (threadDelay)
import Faktory.Ent.Batch

spec :: Spec
spec = do
  describe "runBatch" $ do
    it "runs a success job if all in-batch jobs succeed" $ do
      jobs <- workerTestCase $ \producer -> do
        c <- buildJob @Text mempty producer "c"
        void $ runBatch (success c) producer $ do
          void $ batchPerform @Text mempty producer "a"
          void $ batchPerform @Text mempty producer "b"
        -- Give a little time for Faktory to fire the callback
        liftIO $ threadDelay 500000

      jobs `shouldMatchList` ["a", "b", "c", "HALT"]

    it "does not run a success job if all jobs don't succeed" $ do
      jobs <- workerTestCase $ \producer -> do
        c <- buildJob @Text mempty producer "c"
        void $ runBatch (success c) producer $ do
          void $ batchPerform @Text mempty producer "BOOM"
          void $ batchPerform @Text mempty producer "b"
        liftIO $ threadDelay 500000

      jobs `shouldMatchList` ["BOOM", "b", "HALT"]

    it "runs a job on complete" $ do
      jobs <- workerTestCase $ \producer -> do
        c <- buildJob @Text mempty producer "c"
        void $ runBatch (complete c) producer $ do
          void $ batchPerform @Text mempty producer "a"
          void $ batchPerform @Text mempty producer "b"
        liftIO $ threadDelay 500000

      jobs `shouldMatchList` ["a", "b", "c", "HALT"]

    it "runs a job on complete, even if in-batch jobs fail" $ do
      jobs <- workerTestCase $ \producer -> do
        c <- buildJob @Text mempty producer "c"
        void $ runBatch (complete c) producer $ do
          void $ batchPerform @Text mempty producer "BOOM"
          void $ batchPerform @Text mempty producer "b"
        liftIO $ threadDelay 500000

      jobs `shouldMatchList` ["BOOM", "b", "c", "HALT"]

    it "combines duplicate options in last-wins fashion" $ do
      jobs <- workerTestCase $ \producer -> do
        c <- buildJob @Text mempty producer "c"
        d <- buildJob @Text mempty producer "d"
        let options = description "foo" <> success c <> success d
        void $ runBatch options producer $ do
          void $ batchPerform @Text mempty producer "a"
          void $ batchPerform @Text mempty producer "b"
        liftIO $ threadDelay 500000

      jobs `shouldMatchList` ["a", "b", "d", "HALT"]

    it "runs success and complete if all Jobs were successful" $ do
      jobs <- workerTestCase $ \producer -> do
        c <- buildJob @Text mempty producer "c"
        d <- buildJob @Text mempty producer "d"
        let options = description "foo" <> complete c <> success d
        void $ runBatch options producer $ do
          void $ batchPerform @Text mempty producer "a"
          void $ batchPerform @Text mempty producer "b"
        liftIO $ threadDelay 500000

      jobs `shouldMatchList` ["a", "b", "c", "d", "HALT"]
