module Faktory.Job
  ( Job
  , JobId
  , JobOptions
  , perform
  , retry
  , once
  , reserveFor
  , queue
  , jobtype
  , at
  , in_
  , custom
  , buildJob
  , newJob
  , jobJid
  , jobArg
  , jobOptions
  , jobRetriesRemaining
  , jobReserveForMicroseconds
  ) where

import Faktory.Prelude

import Data.Aeson
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (Last(..))
import Data.Time (UTCTime)
import Faktory.Client (Client(..))
import Faktory.Connection (ConnectionInfo(..))
import Faktory.JobFailure
import Faktory.JobOptions
import Faktory.Producer (Producer(..), pushJob)
import Faktory.Settings (Namespace, Settings(..))
import GHC.Stack
import System.Random

data Job arg = Job
  { jobJid :: JobId
  , jobAt :: Maybe UTCTime
  -- ^ Will be set based on 'JobOptions' when enqueued
  , jobArgs :: NonEmpty arg
  -- ^ Faktory needs to serialize args as a list, but we like a single-argument
  -- interface so that's what we expose. See @'jobArg'@.
  , jobOptions :: JobOptions
  , jobFailure :: Maybe JobFailure
  }

-- | Perform a Job with the given options
--
-- @
-- 'perform' 'mempty' SomeJob
-- 'perform' ('queue' "SomeQueue") SomeJob
-- 'perform' 'once' SomeJob
-- 'perform' ('at' someTime <> 'once') SomeJob
-- 'perform' ('in_' 10 <> 'once') SomeJob
-- 'perform' ('in_' 10 <> 'retry' 3) SomeJob
-- @
--
perform
  :: (HasCallStack, ToJSON arg) => JobOptions -> Producer -> arg -> IO JobId
perform options producer arg = do
  job <- buildJob (appDefs options) producer arg
  jobJid job <$ pushJob producer job
 where
  appDefs = applyJobOptionsDefaults $ clientSettings $ producerClient producer

applyOptions :: Namespace -> JobOptions -> Job arg -> IO (Job arg)
applyOptions namespace options job = do
  scheduledAt <- getAtFromSchedule options
  let namespacedOptions = namespaceQueue namespace $ jobOptions job <> options
  pure $ job { jobAt = scheduledAt, jobOptions = namespacedOptions }

-- | Construct a 'Job' and apply options and Producer settings
buildJob :: JobOptions -> Producer -> arg -> IO (Job arg)
buildJob options producer arg = applyOptions namespace options =<< newJob arg
 where
  namespace =
    connectionInfoNamespace
      $ settingsConnection
      $ clientSettings
      $ producerClient producer

-- | Construct a 'Job' with default 'JobOptions'
newJob :: arg -> IO (Job arg)
newJob arg = do
  -- Ruby uses 12 random hex
  jobId <- take 12 . randomRs ('a', 'z') <$> newStdGen

  pure Job
    { jobJid = jobId
    , jobAt = Nothing
    , jobArgs = pure arg
    , jobOptions = jobtype "Default"
    , jobFailure = Nothing
    }

jobArg :: Job arg -> arg
jobArg Job {..} = NE.head jobArgs

jobRetriesRemaining :: Job arg -> Int
jobRetriesRemaining job = max 0 $ enqueuedRetry - attemptCount
 where
  enqueuedRetry = maybe faktoryDefaultRetry getLast $ joRetry $ jobOptions job
  attemptCount = maybe 0 ((+ 1) . jfRetryCount) $ jobFailure job

jobReserveForMicroseconds :: Job arg -> Int
jobReserveForMicroseconds =
  maybe faktoryDefaultReserveFor (secondToMicrosecond . fromIntegral . getLast)
    . joReserveFor
    . jobOptions

instance ToJSON args => ToJSON (Job args) where
  toJSON = object . toPairs
  toEncoding = pairs . mconcat . toPairs

toPairs :: (KeyValue a, ToJSON arg) => Job arg -> [a]
toPairs Job {..} =
  [ "jid" .= jobJid
  , "at" .= jobAt
  , "args" .= jobArgs
  , "jobtype" .= joJobtype jobOptions
  , "retry" .= joRetry jobOptions
  , "queue" .= joQueue jobOptions
  , "custom" .= joCustom jobOptions
  , "reserve_for" .= joReserveFor jobOptions
  ]

-- brittany-disable-next-binding

instance FromJSON args => FromJSON (Job args) where
  parseJSON = withObject "Job" $ \o -> Job
    <$> o .: "jid"
    <*> o .:? "at"
    <*> o .: "args"
    <*> parseJSON (Object o)
    <*> o .:? "failure"

type JobId = String

-- | https://github.com/contribsys/faktory/wiki/Job-Errors#the-process
--
-- > By default Faktory will retry a job 25 times
--
faktoryDefaultRetry :: Int
faktoryDefaultRetry = 25

faktoryDefaultReserveFor :: Int
faktoryDefaultReserveFor = secondToMicrosecond 1800

secondToMicrosecond :: Int -> Int
secondToMicrosecond n = n * (10 ^ (6 :: Int))
