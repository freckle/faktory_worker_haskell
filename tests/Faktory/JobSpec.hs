{-# LANGUAGE QuasiQuotes #-}

module Faktory.JobSpec
  ( spec
  ) where

import Faktory.Prelude

import Data.Aeson
import Data.Aeson.QQ
import Data.Time (getCurrentTime)
import Faktory.Job
import Test.Hspec

spec :: Spec
spec = do
  -- https://github.com/contribsys/faktory/issues/374#issuecomment-902075572
  describe "jobRetriesRemaining" $ do
    it "handles first consumed" $ do
      job <- decodeJob [aesonQQ|
        { "jid": "abc"
        , "args": [""]
        , "retry": 2
        }
      |]

      jobRetriesRemaining job `shouldBe` 2

    it "handles a first retry" $ do
      now <- getCurrentTime
      job <- decodeJob [aesonQQ|
        { "jid": "abc"
        , "args": [""]
        , "retry": 2
        , "failure":
          { "retry_count": 0
          , "failed_at": #{now}
          }
        }
      |]

      jobRetriesRemaining job `shouldBe` 1

    it "handles a final retry" $ do
      now <- getCurrentTime
      job <- decodeJob [aesonQQ|
        { "jid": "abc"
        , "args": [""]
        , "retry": 2
        , "failure":
          { "retry_count": 1
          , "failed_at": #{now}
          }
        }
      |]

      jobRetriesRemaining job `shouldBe` 0

    it "uses Faktory's default" $ do
      job <- decodeJob [aesonQQ|
        { "jid": "abc"
        , "args": [""]
        }
      |]

      jobRetriesRemaining job `shouldBe` 25

    it "handles retry -1" $ do
      now <- getCurrentTime
      job1 <- decodeJob [aesonQQ|
        { "jid": "abc"
        , "args": [""]
        , "retry": -1
        }
      |]
      job2 <- decodeJob [aesonQQ|
        { "jid": "abc"
        , "args": [""]
        , "retry": -1
        , "failure":
          { "retry_count": 1
          , "failed_at": #{now}
          }
        }
      |]

      jobRetriesRemaining job1 `shouldBe` 0
      jobRetriesRemaining job2 `shouldBe` 0

    it "handles retry 0" $ do
      now <- getCurrentTime
      job1 <- decodeJob [aesonQQ|
        { "jid": "abc"
        , "args": [""]
        , "retry": 0
        }
      |]
      job2 <- decodeJob [aesonQQ|
        { "jid": "abc"
        , "args": [""]
        , "retry": 0
        , "failure":
          { "retry_count": 1
          , "failed_at": #{now}
          }
        }
      |]

      jobRetriesRemaining job1 `shouldBe` 0
      jobRetriesRemaining job2 `shouldBe` 0

    it "handles nonsense" $ do
      now <- getCurrentTime
      job <- decodeJob [aesonQQ|
        { "jid": "abc"
        , "args": [""]
        , "retry": 20
        , "failure":
          { "retry_count": 1000
          , "failed_at": #{now}
          }
        }
      |]

      jobRetriesRemaining job `shouldBe` 0

decodeJob :: Value -> IO (Job Text)
decodeJob v = case fromJSON v of
  Error err -> do
    expectationFailure $ "Job JSON did not parse: " <> err
    error "unreachable" -- I hate that hspec makes ^ IO () and not IO a
  Success job -> pure job
