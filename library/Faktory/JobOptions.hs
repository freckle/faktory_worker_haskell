{-# LANGUAGE DerivingVia #-}

module Faktory.JobOptions
  ( JobOptions(..)

  -- * Modifiers
  , retry
  , once
  , reserveFor
  , queue
  , jobtype
  , at
  , in_
  , custom

  -- * Enqueue-time modifiers
  , getAtFromSchedule
  , namespaceQueue
  ) where

import Faktory.Prelude

import Data.Aeson
import Data.Semigroup (Last(..))
import Data.Semigroup.Generic
import Data.Time
import Faktory.Job.Custom
import Faktory.Settings.Queue (Namespace, Queue)
import qualified Faktory.Settings.Queue as Settings
import GHC.Generics
import Numeric.Natural (Natural)

-- | Options for the execution of a job
--
-- These can be constructed using '(<>)':
--
-- @
-- let options = 'retry' 1 <> 'jobtype' "MyJob"
-- @
--
-- To enqueue with defaults, use 'mempty'.
--
-- Options use 'Last' semantics, so (e.g.) @'retry' x <>@ will set retries to
-- @x@ only if not already set, and @<> 'retry' x@ will override any
-- already-present retries to @x@.
--
data JobOptions = JobOptions
  { joJobtype :: Maybe (Last String)
  , joRetry :: Maybe (Last Int)
  , joQueue :: Maybe (Last Queue)
  , joSchedule :: Maybe (Last (Either UTCTime NominalDiffTime))
  , joCustom :: Maybe Custom
  , joReserveFor :: Maybe (Last Natural)
  }
  deriving stock (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid JobOptions

-- brittany-disable-next-binding

instance FromJSON JobOptions where
  parseJSON = withObject "JobOptions" $ \o ->
    JobOptions
      <$> o .:? "jobtype"
      <*> o .:? "retry"
      <*> o .:? "queue"
      <*> (fmap (Last . Left) <$> o .:? "at")
      <*> o .:? "custom"
      <*> o .:? "reserve_for"

getAtFromSchedule :: JobOptions -> IO (Maybe UTCTime)
getAtFromSchedule options = for (getLast <$> joSchedule options) $ \case
  Left t -> pure t
  Right nd -> addUTCTime nd <$> getCurrentTime

namespaceQueue :: Namespace -> JobOptions -> JobOptions
namespaceQueue namespace options = case joQueue options of
  Nothing -> options
  Just (Last q) -> options <> queue (Settings.namespaceQueue namespace q)

reserveFor :: Natural -> JobOptions
reserveFor n = mempty { joReserveFor = Just $ Last n }

retry :: Int -> JobOptions
retry n = mempty { joRetry = Just $ Last n }

-- | Equivalent to @'retry' (-1)@: no retries, and move to Dead on failure
once :: JobOptions
once = retry (-1)

queue :: Queue -> JobOptions
queue q = mempty { joQueue = Just $ Last q }

jobtype :: String -> JobOptions
jobtype jt = mempty { joJobtype = Just $ Last jt }

at :: UTCTime -> JobOptions
at t = mempty { joSchedule = Just $ Last $ Left t }

in_ :: NominalDiffTime -> JobOptions
in_ i = mempty { joSchedule = Just $ Last $ Right i }

custom :: ToJSON a => a -> JobOptions
custom v = mempty { joCustom = Just $ toCustom v }
