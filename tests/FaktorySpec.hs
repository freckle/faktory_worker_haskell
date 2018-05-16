module FaktorySpec
  ( spec
  ) where

import Faktory.Prelude

import Control.Concurrent.MVar
import Data.Maybe
import Faktory.Client
import Faktory.Settings
import Faktory.Worker
import System.Environment
import Test.Hspec

spec :: Spec
spec = describe "Faktory" $ do
  it "can push and process jobs" $ do
    settings <- getTestSettings
    bracket (newClient settings Nothing) closeClient $ \client -> do
      void $ pushJob @Text client defaultQueue "a"
      void $ pushJob @Text client defaultQueue "b"
      void $ pushJob @Text client defaultQueue "HALT"

    processedJobs <- newMVar ([] :: [Text])
    runWorker settings defaultQueue $ \job -> do
      modifyMVar_ processedJobs $ pure . (job:)
      when (job == "HALT") $ throw WorkerHalt

    jobs <- readMVar processedJobs
    jobs `shouldMatchList` ["a", "b", "HALT"]

-- TODO: Add proper, complete FAKTORY_URL support, as per Faktory documentation
getTestSettings :: IO Settings
getTestSettings = do
  mFaktoryHost <- lookupEnv "FAKTORY_HOST"

  let
    defaultConnection = settingsConnection defaultSettings
    defaultHostName = connectionHostName defaultConnection

    testHostName = fromMaybe defaultHostName mFaktoryHost
    testConnection = defaultConnection
      { connectionHostName = testHostName
      }

  pure defaultSettings { settingsConnection = testConnection }
