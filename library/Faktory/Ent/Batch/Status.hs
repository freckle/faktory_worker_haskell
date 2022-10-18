module Faktory.Ent.Batch.Status
  ( jobBatchId
  , BatchStatus(..)
  , batchStatus
  ) where

import Faktory.Prelude

import Control.Applicative ((<|>))
import Control.Error.Util (hush)
import Data.Aeson
import Data.ByteString.Lazy as BSL
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Faktory.Client
import Faktory.Ent.Batch
import Faktory.Job (Job, jobOptions)
import Faktory.Job.Custom
import Faktory.JobOptions (JobOptions(..))
import Faktory.Producer
import GHC.Generics

data BatchStatus = BatchStatus
  { bid :: BatchId
  , total :: Int
  , pending :: Int
  , failed :: Int
  , created_at :: UTCTime
  , description :: Text
  }
  deriving stock Generic
  deriving anyclass FromJSON

newtype ReadCustomBatchId = ReadCustomBatchId
  { _bid :: BatchId
  }
  deriving stock (Show,Eq,Generic)

instance FromJSON ReadCustomBatchId where
  -- Faktory seems to use the key '_bid' when enqueuing callback jobs and 'bid' for normal jobs...
  parseJSON v = withParser "_bid" v <|> withParser "bid" v
   where
    withParser s =
      withObject "ReadCustomBatchId" $ \o -> ReadCustomBatchId <$> o .: s

jobBatchId :: Job arg -> Maybe BatchId
jobBatchId job = do
  custom <- joCustom $ jobOptions job
  _bid <$> hush (fromCustom custom)

batchStatus :: Producer -> BatchId -> IO (Either String (Maybe BatchStatus))
batchStatus producer (BatchId bid) = commandJSON
  (producerClient producer)
  "BATCH STATUS"
  [BSL.fromStrict $ encodeUtf8 bid]
