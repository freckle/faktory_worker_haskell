module FaktorySpec
  ( spec
  ) where

import Faktory.Prelude

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Faktory.Job
import Faktory.Producer
import Faktory.Settings
import Faktory.Worker
import Test.Hspec

spec :: Spec
spec = describe "Faktory" $ do
  it "can push and process jobs" $ do
    bracket newProducerEnv closeProducer $ \producer -> do
      void $ flush producer
      void $ perform @Text mempty producer "a"
      void $ perform @Text mempty producer "b"
      void $ perform @Text mempty producer "HALT"

    processedJobs <- newMVar ([] :: [Text])
    runWorkerEnv $ \job -> do
      modifyMVar_ processedJobs $ pure . (job :)
      when (job == "HALT") $ throw WorkerHalt

    jobs <- readMVar processedJobs
    jobs `shouldMatchList` ["a", "b", "HALT"]

  it "can push jobs with optional attributes" $ do
    bracket newProducerEnv closeProducer $ \producer -> do
      void $ flush producer
      void $ perform @Text once producer "a"
      void $ perform @Text (retry 0) producer "b"
      void $ perform @Text mempty producer "HALT"

    processedJobs <- newMVar ([] :: [Text])
    runWorkerEnv $ \job -> do
      modifyMVar_ processedJobs $ pure . (job :)
      when (job == "HALT") $ throw WorkerHalt

    jobs <- readMVar processedJobs
    jobs `shouldMatchList` ["a", "b", "HALT"]

  it "correctly handles fetch timeouts" $ do
    settings <- envSettings
    workerSettings' <- envWorkerSettings
    let workerSettings = workerSettings' { settingsIdleDelay = 0 }

    -- start a background thread that waits for longer than the fetch timeout,
    -- then stops the worker.
    --
    -- https://github.com/contribsys/faktory/wiki/Worker-Lifecycle#fetching-jobs
    --
    -- This ensures that the worker loop experiences recieving a Nothing from
    -- the Server and handles it correctly. Setting our own idle delay to 0
    -- ensures that we'll pick up the following HALT message immediately.
    --
    void $ forkIO $ bracket (newProducer settings) closeProducer $ \producer ->
      do
        void $ flush producer
        threadDelay $ 2 * 1000000 + 250000
        void $ perform @Text mempty producer "HALT"

    processedJobs <- newMVar ([] :: [Text])
    runWorker settings workerSettings $ \job -> do
      modifyMVar_ processedJobs $ pure . (job :)
      when (job == "HALT") $ throw WorkerHalt

    jobs <- readMVar processedJobs
    jobs `shouldMatchList` ["HALT"]
