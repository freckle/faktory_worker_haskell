module Faktory.Ent.BatchSpec
  ( spec
  ) where

import Faktory.Test

import Control.Concurrent (threadDelay)
import Control.Monad.Reader
import Faktory.Ent.Batch
import qualified Faktory.Ent.Batch.Status as BatchStatus

spec :: Spec
spec = do
  describe "runBatch" $ do
    it "runs a success job if all in-batch jobs succeed" $ do
      jobs <- workerTestCase $ \producer -> do
        c <- buildJob mempty producer $ J "c"
        void $ runBatch (success c) producer $ do
          void $ batchPerform mempty producer $ J "a"
          void $ batchPerform mempty producer $ J "b"
        -- Give a little time for Faktory to fire the callback
        liftIO $ threadDelay 500000

      jobs `shouldMatchList` ["a", "b", "c", Halt]

    it "does not run a success job if all jobs don't succeed" $ do
      jobs <- workerTestCase $ \producer -> do
        c <- buildJob mempty producer $ J "c"
        void $ runBatch (success c) producer $ do
          void $ batchPerform mempty producer Boom
          void $ batchPerform mempty producer $ J "b"
        liftIO $ threadDelay 500000

      jobs `shouldMatchList` [Boom, "b", Halt]

    it "runs a job on complete" $ do
      jobs <- workerTestCase $ \producer -> do
        c <- buildJob mempty producer $ J "c"
        void $ runBatch (complete c) producer $ do
          void $ batchPerform mempty producer $ J "a"
          void $ batchPerform mempty producer $ J "b"
        liftIO $ threadDelay 500000

      jobs `shouldMatchList` ["a", "b", "c", Halt]

    it "runs a job on complete, even if in-batch jobs fail" $ do
      jobs <- workerTestCase $ \producer -> do
        c <- buildJob mempty producer $ J "c"
        void $ runBatch (complete c) producer $ do
          void $ batchPerform mempty producer Boom
          void $ batchPerform mempty producer $ J "b"
        liftIO $ threadDelay 500000

      jobs `shouldMatchList` [Boom, "b", "c", Halt]

    it "combines duplicate options in last-wins fashion" $ do
      jobs <- workerTestCase $ \producer -> do
        c <- buildJob mempty producer $ J "c"
        d <- buildJob mempty producer $ J "d"
        let options = description "foo" <> success c <> success d
        void $ runBatch options producer $ do
          void $ batchPerform mempty producer $ J "a"
          void $ batchPerform mempty producer $ J "b"
        liftIO $ threadDelay 500000

      jobs `shouldMatchList` ["a", "b", "d", Halt]

    it "runs success and complete if all Jobs were successful" $ do
      jobs <- workerTestCase $ \producer -> do
        c <- buildJob mempty producer $ J "c"
        d <- buildJob mempty producer $ J "d"
        let options = description "foo" <> complete c <> success d
        void $ runBatch options producer $ do
          void $ batchPerform mempty producer $ J "a"
          void $ batchPerform mempty producer $ J "b"
        liftIO $ threadDelay 500000

      jobs `shouldMatchList` ["a", "b", "c", "d", Halt]

    it "supports BATCH STATUS" $ do
      batchId <- withWorker id $ withProducer $ \producer -> do
        c <- buildJob mempty producer $ J "c"
        d <- buildJob mempty producer $ J "d"
        let options = description "foo" <> complete c <> success d
        batchId <- runBatch options producer $ do
          void $ batchPerform mempty producer $ J "a"
          void $ batchPerform mempty producer $ J "b"
          ask
        batchId <$ liftIO (threadDelay 500000)

      emStatus <- bracket newProducerEnv closeProducer
        $ \producer -> BatchStatus.batchStatus producer batchId

      fmap (fmap BatchStatus.description) emStatus `shouldBe` Right (Just "foo")
      fmap (fmap BatchStatus.total) emStatus `shouldBe` Right (Just 2)
      fmap (fmap BatchStatus.pending) emStatus `shouldBe` Right (Just 0)
      fmap (fmap BatchStatus.failed) emStatus `shouldBe` Right (Just 0)
