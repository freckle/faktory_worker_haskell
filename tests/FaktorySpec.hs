module FaktorySpec
  ( spec
  ) where

import Faktory.Prelude

import Control.Concurrent.MVar
import Faktory.Client
import Faktory.Settings
import Faktory.Worker
import Test.Hspec

spec :: Spec
spec = describe "Faktory" $ do
  it "can push and process jobs" $ do
    settings <- envSettings
    bracket (newClient settings Nothing) closeClient $ \client -> do
      void $ flush client
      void $ pushJob @Text client defaultQueue "a"
      void $ pushJob @Text client defaultQueue "b"
      void $ pushJob @Text client defaultQueue "HALT"

    processedJobs <- newMVar ([] :: [Text])
    runWorker settings defaultQueue $ \job -> do
      modifyMVar_ processedJobs $ pure . (job:)
      when (job == "HALT") $ throw WorkerHalt

    jobs <- readMVar processedJobs
    jobs `shouldMatchList` ["a", "b", "HALT"]
