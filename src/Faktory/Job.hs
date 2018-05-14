{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Faktory.Job
  ( Job
  , JobId
  , newJob
  , jobJid
  , jobArg
  ) where

import Data.Aeson
import Data.Aeson.Casing
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Time
import Faktory.Settings (Queue)
import GHC.Generics
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

newJob :: ToJSON arg => Queue -> arg -> IO (Job arg)
newJob queue arg = do
  -- Ruby uses 12 random hex
  jobId <- take 12 . randomRs ('a', 'z') <$> newStdGen

  pure Job
    { jobJid = jobId
    , jobRetry = 25
    , jobQueue = queue
    , jobJobtype = "Example" -- TODO
    , jobAt = Nothing -- TODO
    , jobArgs = pure arg
    }

jobArg :: Job arg -> arg
jobArg Job{..} = NonEmpty.head jobArgs

instance ToJSON args => ToJSON (Job args) where
   toJSON = genericToJSON $ aesonPrefix snakeCase
   toEncoding = genericToEncoding $ aesonPrefix snakeCase

instance FromJSON args => FromJSON (Job args) where
   parseJSON = genericParseJSON $ aesonPrefix snakeCase

type JobId = String
