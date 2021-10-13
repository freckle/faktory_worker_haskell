module FaktorySpec
  ( spec
  ) where

import Faktory.Test

import Control.Concurrent (threadDelay)
import Data.Time (getCurrentTime)
import Faktory.Settings

spec :: Spec
spec = describe "Faktory" $ do
  it "can push and process jobs" $ do
    jobs <- workerTestCase $ \producer -> do
      void $ perform mempty producer $ J "a"
      void $ perform mempty producer $ J "b"

    jobs `shouldMatchList` ["a", "b", Halt]

  it "can push jobs with optional attributes" $ do
    jobs <- workerTestCase $ \producer -> do
      void $ perform once producer $ J "a"
      void $ perform (retry 0) producer $ J "b"

    jobs `shouldMatchList` ["a", "b", Halt]

  it "can push Jobs to run at a given time" $ do
    now <- getCurrentTime
    jobs <- workerTestCase $ \producer -> do
      void $ perform (at now) producer $ J "a"

    jobs `shouldMatchList` ["a", Halt]

  it "can push Jobs to run in a given amount of seconds" $ do
    jobs <- workerTestCase $ \producer -> do
      void $ perform (in_ 0) producer $ J "a"

    jobs `shouldMatchList` ["a", Halt]

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

    jobs `shouldMatchList` [Halt]

  it "does not process jobs when reserve_for timeout expires" $ do
    jobs <- workerTestCase $ \producer -> do
      void $ perform (reserveFor 1) producer Wait

    jobs `shouldMatchList` [Halt]

  it "processes jobs within reserve_for window" $ do
    jobs <- workerTestCase $ \producer -> do
      void $ perform (reserveFor 4) producer Wait

    jobs `shouldMatchList` [Wait, Halt]
