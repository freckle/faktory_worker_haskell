module Faktory.Job
  ( Job
  , JobId
  , perform
  , retry
  , once
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
  , jobRetry :: Int
  , jobQueue :: Queue
  , jobJobtype :: String
  , jobAt :: Maybe UTCTime
  , jobArgs :: NonEmpty arg
  -- ^ Faktory needs to serialize args as a list, but we like a single-argument
  -- interface so that's what we expose. See @'jobArg'@.
  }
  deriving Generic

-- | Individual changes to a @'Job'@ to be 'perform'ed
data JobUpdate
  = SetRetry Int
  | SetAt UTCTime
  | SetIn NominalDiffTime

newtype JobOptions = JobOptions [JobUpdate]
  deriving newtype (Semigroup, Monoid)

-- | Perform a Job with the given options
--
-- @
-- 'perform' 'mempty' "queue" SomeJob
-- 'perform' 'once' "queue" SomeJob
-- 'perform' ('retry' 3 <> 'in_' 10) "queue" SomeJob
-- 'perform' ('once' <> 'at' someTime) "queue" SomeJob
-- 'perform' ('once' <> 'in_' 10) "queue" SomeJob
-- @
--
perform
  :: (HasCallStack, ToJSON arg)
  => JobOptions
  -> Client
  -> Queue
  -> arg
  -> IO JobId
perform options client queue arg = do
  job <- applyOptions options =<< newJob queue arg
  jobJid job <$ pushJob client job

applyOptions :: JobOptions -> Job arg -> IO (Job arg)
applyOptions (JobOptions patches) = go patches
 where
  go [] job = pure job
  go (set : sets) job = case set of
    SetRetry n -> go sets $ job { jobRetry = n }
    SetAt time -> go sets $ job { jobAt = Just time }
    SetIn diff -> do
      now <- getCurrentTime
      go sets $ job { jobAt = Just $ addUTCTime diff now }

retry :: Int -> JobOptions
retry n = JobOptions [SetRetry n]

once :: JobOptions
once = retry 0

at :: UTCTime -> JobOptions
at t = JobOptions [SetAt t]

in_ :: NominalDiffTime -> JobOptions
in_ i = JobOptions [SetIn i]

newJob :: ToJSON arg => Queue -> arg -> IO (Job arg)
newJob queue arg = do
  -- Ruby uses 12 random hex
  jobId <- take 12 . randomRs ('a', 'z') <$> newStdGen

  pure Job
    { jobJid = jobId
    , jobRetry = 25
    , jobQueue = queue
    , jobJobtype = "Example" -- TODO
    , jobAt = Nothing
    , jobArgs = pure arg
    }

jobArg :: Job arg -> arg
jobArg Job {..} = NE.head jobArgs

instance ToJSON args => ToJSON (Job args) where
   toJSON = genericToJSON $ aesonPrefix snakeCase
   toEncoding = genericToEncoding $ aesonPrefix snakeCase

instance FromJSON args => FromJSON (Job args) where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

type JobId = String
