module Faktory.JobFailure
  ( JobFailure (..)
  ) where

import Faktory.Prelude

import Data.Aeson
import Data.Time (UTCTime)

data JobFailure = JobFailure
  { jfRetryCount :: Int
  , jfFailedAt :: UTCTime
  , jfNextAt :: Maybe UTCTime
  , jfErrorMessage :: Maybe Text
  , jfErrorType :: Maybe Text
  , jfBacktrace :: Maybe [Text]
  }

-- brittany-disable-next-binding

instance FromJSON JobFailure where
  parseJSON = withObject "Failure" $ \o ->
    JobFailure
      <$> o .: "retry_count"
      <*> o .: "failed_at"
      <*> o .:? "next_at"
      <*> o .:? "error_message"
      <*> o .:? "error_type"
      <*> o .:? "backtrace"
