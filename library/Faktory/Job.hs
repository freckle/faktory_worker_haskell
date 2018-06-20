module Faktory.Job
  ( Job
  , JobId
  , perform
  , performAt
  , performIn
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
perform client queue arg = do
  job <- newJob queue arg
  jobJid job <$ pushJob client job

performAt
  :: (HasCallStack, ToJSON arg) => Client -> Queue -> UTCTime -> arg -> IO JobId
performAt client queue time arg = do
  job <- newJob queue arg
  jobJid job <$ pushJob client job { jobAt = Just time }

performIn
  :: (HasCallStack, ToJSON arg)
  => Client
  -> Queue
  -> NominalDiffTime
  -> arg
  -> IO JobId
performIn client queue diff arg = do
  time <- addUTCTime diff <$> getCurrentTime
  performAt client queue time arg

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
