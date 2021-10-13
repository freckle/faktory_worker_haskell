module Faktory.JobOptionsSpec
  ( spec
  ) where

import Faktory.Prelude

import Data.Aeson
import Data.Semigroup (Last(..))
import Data.Time
  ( DiffTime
  , UTCTime(..)
  , addUTCTime
  , diffTimeToPicoseconds
  , getCurrentTime
  , secondsToDiffTime
  )
import Data.Time.Calendar (fromGregorian)
import Faktory.Job.Custom
import Faktory.JobOptions
import Faktory.Settings (Namespace(..), Queue(..), defaultSettings)
import Test.Hspec

spec :: Spec
spec = do
  describe "JobOptions" $ do
    describe "(<>)" $ do
      it "uses Last semantics" $ do
        let options = retry 1 <> retry 2

        fmap getLast (joRetry options) `shouldBe` Just 2

      it "merges custom fields" $ do
        let
          custom1 = object ["a" .= True, "b" .= True, "c" .= True]
          custom2 = object ["a" .= False, "d" .= False]
          options = custom custom1 <> custom custom2

        joCustom options
          `shouldBe` Just
                       (toCustom
                       $ object
                           [ "a" .= False
                           , "b" .= True
                           , "c" .= True
                           , "d" .= False
                           ]
                       )

    describe "getAtFromSchedule" $ do
      it "sets at based on in" $ do
        now <- getCurrentTime
        let time = addUTCTime 30 now

        timeAt <- getAtFromSchedule $ in_ 30

        fmap truncateUTCTime timeAt `shouldBe` Just (truncateUTCTime time)

      it "respects the last option given" $ do
        now <- getCurrentTime
        let time = makeUTCTime 2020 1 1 0

        timeAt1 <- getAtFromSchedule $ in_ 5 <> at time
        timeAt2 <- getAtFromSchedule $ at time <> in_ 5

        timeAt1 `shouldBe` Just time
        fmap truncateUTCTime timeAt2
          `shouldBe` Just (truncateUTCTime $ addUTCTime 5 now)

    describe "namespaceQueue" $ do
      it "leaves no queue alone" $ do
        let options = namespaceQueue (Namespace "namespace.") mempty

        fmap getLast (joQueue options) `shouldBe` Nothing

      it "namespaces a queue when given" $ do
        let options = namespaceQueue (Namespace "namespace.") $ queue "queue"

        fmap getLast (joQueue options) `shouldBe` Just (Queue "namespace.queue")

    describe "applyJobOptionsDefaults" $ do
      it "applies defaults to empty settings" $ do
        let options = applyJobOptionsDefaults defaultSettings mempty

        fmap getLast (joQueue options) `shouldBe` Just "default"
        fmap getLast (joRetry options) `shouldBe` Just 25

      it "doesn't override provided queue" $ do
        let options = applyJobOptionsDefaults defaultSettings $ queue "foo"

        fmap getLast (joQueue options) `shouldBe` Just "foo"
        fmap getLast (joRetry options) `shouldBe` Just 25

      it "doesn't override provided retry" $ do
        let options = applyJobOptionsDefaults defaultSettings $ retry 3

        fmap getLast (joQueue options) `shouldBe` Just "default"
        fmap getLast (joRetry options) `shouldBe` Just 3


makeUTCTime :: Integer -> Int -> Int -> DiffTime -> UTCTime
makeUTCTime y m d s =
  UTCTime { utctDay = fromGregorian y m d, utctDayTime = s }

truncateUTCTime :: UTCTime -> UTCTime
truncateUTCTime t = t
  { utctDayTime =
    secondsToDiffTime
    $ picosecondsToSeconds
    $ diffTimeToPicoseconds
    $ utctDayTime t
  }

-- sameSecondAs :: UTCTime -> UTCTime -> Bool
-- sameSecondAs a b =
--   (utctDay a == utctDay b)
--     &&
--     == picosecondsToSeconds (diffTimeToPicoseconds $ utctDayTime b)

picosecondsToSeconds :: Integer -> Integer
picosecondsToSeconds = round @Double . (/ 1000000000000) . fromIntegral
