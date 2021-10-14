{-# LANGUAGE DerivingVia #-}

-- | Support for the @BATCH@ command (Enterprise only)
--
-- <https://github.com/contribsys/faktory/wiki/Ent-Batches>
--
-- Batches allow multiple Jobs to be enqueued as a group, with a description
-- (visible in the admin UI) and Jobs attached to run on completion of all Jobs
-- within the group (always, or only if all were successful).
--
-- Usage:
--
-- @
-- -- Build a Job to run at completion of the Batch. Arguments are the same as
-- -- you would pass to 'perform' the Job.
-- onComplete <- buildJob mempty producer myJob
--
-- 'runBatch' ('complete' onComplete <> 'description' "My Batch") producer $ do
--   -- Use 'batchPerform' instead of 'perform'
--   void $ 'batchPerform' mempty producer myBatchedJob1
--   void $ 'batchPerform' mempty producer myBatchedJob2
-- @
--
-- __/NOTE__: This module does not support batched Jobs dynamically adding more
-- Jobs to the Batch. PRs welcome.
--
module Faktory.Ent.Batch
  (
  -- * Options
    BatchOptions
  , description
  , complete
  , success

  -- * Running
  , runBatch
  , Batch
  , batchPerform

  -- * Low-level
  , BatchId(..)
  , CustomBatchId(..)
  , newBatch
  , commitBatch
  ) where

import Faktory.Prelude

import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Casing
import Data.ByteString.Lazy as BSL
import Data.Semigroup (Last(..))
import Data.Semigroup.Generic
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Faktory.Client
import Faktory.Job
import Faktory.Producer
import GHC.Generics
import GHC.Stack
import Data.Data (Data)

newtype Batch a = Batch (ReaderT BatchId IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader BatchId)

newtype BatchId = BatchId Text
  deriving newtype (FromJSON, ToJSON)

data BatchOptions arg = BatchOptions
  { boDescription :: Maybe (Last Text)
  , boSuccess :: Maybe (Last (Job arg))
  , boComplete :: Maybe (Last (Job arg))
  }
  deriving stock Generic
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid (BatchOptions arg)

instance ToJSON arg => ToJSON (BatchOptions arg) where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

description :: Text -> BatchOptions arg
description d = mempty { boDescription = Just $ Last d }

complete :: Job arg -> BatchOptions arg
complete job = mempty { boComplete = Just $ Last job }

success :: Job arg -> BatchOptions arg
success job = mempty { boSuccess = Just $ Last job }

runBatch :: ToJSON arg => BatchOptions arg -> Producer -> Batch a -> IO a
runBatch options producer (Batch f) = do
  bid <- newBatch producer options
  result <- runReaderT f bid
  result <$ commitBatch producer bid

newtype CustomBatchId = CustomBatchId
  { bid :: BatchId
  }
  deriving stock Generic
  deriving anyclass ToJSON

batchPerform
  :: (HasCallStack, Data arg, ToJSON arg) => JobOptions -> Producer -> arg -> Batch JobId
batchPerform options producer arg = do
  bid <- ask
  Batch $ lift $ perform (options <> custom (CustomBatchId bid)) producer arg

newBatch :: ToJSON arg => Producer -> BatchOptions arg -> IO BatchId
newBatch producer options = do
  result <- commandByteString
    (producerClient producer)
    "BATCH NEW"
    [encode options]
  case result of
    Left err -> batchNewError err
    Right Nothing -> batchNewError "No BatchId returned"
    Right (Just bs) -> pure $ BatchId $ decodeUtf8 $ BSL.toStrict bs
  where batchNewError err = throwString $ "BATCH NEW error: " <> err

commitBatch :: Producer -> BatchId -> IO ()
commitBatch producer (BatchId bid) = command_
  (producerClient producer)
  "BATCH COMMIT"
  [BSL.fromStrict $ encodeUtf8 bid]
