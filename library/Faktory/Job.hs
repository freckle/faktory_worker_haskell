module Faktory.Job
  ( Job
  , JobId
  , perform
  , performAt
  , performIn
  , performOnce
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
  -- ^ Faktory needs to serialized args as a list, but we like a single-argument
  -- interface so that's what we expose. See @'jobArg'@.
  }
  deriving Generic

perform :: (HasCallStack, ToJSON arg) => Client -> Queue -> arg -> IO JobId
perform = performWith id

-- | Perform the Job at the given time
performAt
  :: (HasCallStack, ToJSON arg) => UTCTime -> Client -> Queue -> arg -> IO JobId
performAt time = performWith $ \job -> job { jobAt = Just time }

-- | Perform the Job in some time, given as a @'NominalDiffTime'@
performIn
  :: (HasCallStack, ToJSON arg)
  => NominalDiffTime
  -> Client
  -> Queue
  -> arg
  -> IO JobId
performIn diff = performWithM $ \job -> do
  time <- addUTCTime diff <$> getCurrentTime
  pure job { jobAt = Just time }

-- | Perform the Job with 0 retries
performOnce :: (HasCallStack, ToJSON arg) => Client -> Queue -> arg -> IO JobId
performOnce = performWith $ \job -> job { jobRetry = 0 }

performWith
  :: (HasCallStack, ToJSON arg)
  => (Job arg -> Job arg)
  -> Client
  -> Queue
  -> arg
  -> IO JobId
performWith f client queue arg = do
  job <- f <$> newJob queue arg
  jobJid job <$ pushJob client job

performWithM
  :: (HasCallStack, ToJSON arg)
  => (Job arg -> IO (Job arg))
  -> Client
  -> Queue
  -> arg
  -> IO JobId
performWithM f client queue arg = do
  job <- f =<< newJob queue arg
  jobJid job <$ pushJob client job

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
jobArg Job{..} = NE.head jobArgs

instance ToJSON args => ToJSON (Job args) where
   toJSON = genericToJSON $ aesonPrefix snakeCase
   toEncoding = genericToEncoding $ aesonPrefix snakeCase

instance FromJSON args => FromJSON (Job args) where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

type JobId = String
