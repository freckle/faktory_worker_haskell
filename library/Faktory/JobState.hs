module Faktory.JobState
  ( JobState(..)
  ) where

import Faktory.Prelude

import Control.Arrow ((&&&))
import Control.Error.Util (note)
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T

data JobState
  = JobStateUnknown
  | JobStateEnqueued
  | JobStateWorking
  | JobStateSuccess
  | JobStateFailed
  | JobStateDead
  deriving stock (Eq, Show, Bounded, Enum)

instance ToJSON JobState where
  toJSON = toJSON . jobStateToText
  toEncoding = toEncoding . jobStateToText

instance FromJSON JobState where
  parseJSON = withText "JobState" $ either fail pure . jobStateFromText

jobStateToText :: JobState -> Text
jobStateToText = \case
  JobStateUnknown -> "unknown"
  JobStateEnqueued -> "enqueued"
  JobStateWorking -> "working"
  JobStateSuccess -> "success"
  JobStateFailed -> "failed"
  JobStateDead -> "dead"

jobStateFromText :: Text -> Either String JobState
jobStateFromText x =
  note
      (unpack
      $ "Invalid JobState: "
      <> x
      <> ", must be one of "
      <> T.intercalate ", " (HashMap.keys jobStateMap)
      )
    $ HashMap.lookup x jobStateMap

jobStateMap :: HashMap Text JobState
jobStateMap =
  HashMap.fromList $ map (jobStateToText &&& id) [minBound .. maxBound]
{-# NOINLINE jobStateMap #-}
