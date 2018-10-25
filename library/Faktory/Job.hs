module Faktory.Job
  ( Job
  , JobId
  , perform
  , retry
  , once
  , queue
  , jobtype
  , at
  , in_
  , newJob
  , jobJid
  , jobArg
  ) where

import Faktory.Prelude

import Data.Aeson
import Data.Aeson.Casing
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Time
import Faktory.Client (Client, pushJob)
import Faktory.Settings (Queue)
import GHC.Generics
import GHC.Stack
import System.Random

data Job arg = Job
  { jobJid :: JobId
  , jobJobtype :: String
  , jobArgs :: NonEmpty arg
  -- ^ Faktory needs to serialize args as a list, but we like a single-argument
  -- interface so that's what we expose. See @'jobArg'@.
  , jobRetry :: Maybe Int
  , jobQueue :: Maybe Queue
  , jobAt :: Maybe UTCTime
  }
  deriving Generic

-- | Individual changes to a @'Job'@ to be 'perform'ed
data JobUpdate
  = SetRetry Int
  | SetQueue Queue
  | SetJobtype String
  | SetAt UTCTime
  | SetIn NominalDiffTime

newtype JobOptions = JobOptions [JobUpdate]
  deriving newtype (Semigroup, Monoid)

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
perform :: (HasCallStack, ToJSON arg) => JobOptions -> Client -> arg -> IO JobId
perform options client arg = do
  job <- applyOptions options =<< newJob arg
  jobJid job <$ pushJob client job

applyOptions :: JobOptions -> Job arg -> IO (Job arg)
applyOptions (JobOptions patches) = go patches
 where
  go [] job = pure job
  go (set : sets) job = case set of
    SetRetry n -> go sets $ job { jobRetry = Just n }
    SetQueue q -> go sets $ job { jobQueue = Just q }
    SetJobtype jt -> go sets $ job { jobJobtype = jt }
    SetAt time -> go sets $ job { jobAt = Just time }
    SetIn diff -> do
      now <- getCurrentTime
      go sets $ job { jobAt = Just $ addUTCTime diff now }

retry :: Int -> JobOptions
retry n = JobOptions [SetRetry n]

-- | Equivalent to @'retry' (-1)@: no retries, and move to Dead on failure
once :: JobOptions
once = retry (-1)

queue :: Queue -> JobOptions
queue q = JobOptions [SetQueue q]

jobtype :: String -> JobOptions
jobtype jt = JobOptions [SetJobtype jt]

at :: UTCTime -> JobOptions
at t = JobOptions [SetAt t]

in_ :: NominalDiffTime -> JobOptions
in_ i = JobOptions [SetIn i]

newJob :: arg -> IO (Job arg)
newJob arg = do
  -- Ruby uses 12 random hex
  jobId <- take 12 . randomRs ('a', 'z') <$> newStdGen

  pure Job
    { jobJid = jobId
    , jobJobtype = "Default"
    , jobArgs = pure arg
    , jobRetry = Nothing
    , jobQueue = Nothing
    , jobAt = Nothing
    }

jobArg :: Job arg -> arg
jobArg Job {..} = NE.head jobArgs

instance ToJSON args => ToJSON (Job args) where
   toJSON = genericToJSON $ aesonPrefix snakeCase
   toEncoding = genericToEncoding $ aesonPrefix snakeCase

instance FromJSON args => FromJSON (Job args) where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

type JobId = String
