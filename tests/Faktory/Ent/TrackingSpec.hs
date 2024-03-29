{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Faktory.Ent.TrackingSpec
  ( spec
  ) where

import Faktory.Test

import Control.Concurrent.MVar
import Faktory.Ent.Tracking

spec :: Spec
spec = do
  describe "tracked" $ do
    it "adds custom option so we can use TRACK GET later" $ do
      var <- newMVar []
      void $ workerTestCase $ \producer -> do
        a <- perform @Text tracked producer "a"
        modifyMVar_ var $ pure . (<> [a])
        b <- perform @Text tracked producer "BOOM"
        modifyMVar_ var $ pure . (<> [b])

      enqueuedJobIds <- readMVar var
      enqueuedJobIds `shouldSatisfy` (== 2) . length

      let [aJid, bJid] = enqueuedJobIds

      (aDetails, bDetails, cDetails) <-
        bracket newProducerEnv closeProducer $ \producer ->
          (,,)
            <$> trackGetHush producer aJid
            <*> trackGetHush producer bJid
            <*> trackGetHush producer "madeUp"

      jdState aDetails `shouldBe` JobStateSuccess
      jdState bDetails `shouldBe` JobStateFailed
      jdState cDetails `shouldBe` JobStateUnknown

  describe "trackSet" $ do
    it "updates Job Details" $ do
      var <- newMVar []
      void $ workerTestCase $ \producer -> do
        a <- perform @Text tracked producer "a"
        modifyMVar_ var $ pure . (<> [a])

      enqueuedJobIds <- readMVar var
      enqueuedJobIds `shouldSatisfy` (== 1) . length

      let [aJid] = enqueuedJobIds

      aDetails <- bracket newProducerEnv closeProducer $ \producer -> do
        trackSet producer
          $ SetJobDetails
            { sjdJid = aJid
            , sjdPercent = Just 100
            , sjdDesc = Just "Updated"
            , sjdReserveUntil = Nothing
            }

        trackGetHush producer aJid

      jdPercent aDetails `shouldBe` Just 100
      jdDesc aDetails `shouldBe` Just "Updated"
