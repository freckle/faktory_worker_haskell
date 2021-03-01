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
-- 'runBatchT' ('complete' onComplete <> 'description' "My Batch") producer $ do
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
    , runBatchT
    , BatchT
    , batchPerform
    )
where

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

newtype BatchT a = BatchT (ReaderT BatchId IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader BatchId)

newtype BatchId = BatchId Text
  deriving newtype ToJSON

data BatchOptions arg = BatchOptions
  { boDescription :: Maybe (Last Text)
  , boSuccess :: Maybe (Last (Job arg))
  , boComplete :: Maybe (Last (Job arg))
  }
  deriving stock Generic
  deriving Semigroup via GenericSemigroupMonoid (BatchOptions arg)

instance ToJSON arg => ToJSON (BatchOptions arg) where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

description :: Text -> BatchOptions arg
description d = BatchOptions
  { boDescription = Just $ Last d
  , boSuccess = Nothing
  , boComplete = Nothing
  }

complete :: Job arg -> BatchOptions arg
complete job = BatchOptions
  { boDescription = Nothing
  , boSuccess = Nothing
  , boComplete = Just $ Last job
  }

success :: Job arg -> BatchOptions arg
success job = BatchOptions
  { boDescription = Nothing
  , boSuccess = Just $ Last job
  , boComplete = Nothing
  }

runBatchT :: ToJSON arg => BatchOptions arg -> Producer -> BatchT a -> IO a
runBatchT options producer (BatchT f) = do
  bid <- newBatch producer options
  result <- runReaderT f bid
  result <$ commitBatch producer bid

newtype CustomBatchId = CustomBatchId
  { bid :: BatchId
  }
  deriving stock Generic
  deriving anyclass ToJSON

batchPerform
  :: (HasCallStack, ToJSON arg) => JobOptions -> Producer -> arg -> BatchT JobId
batchPerform options producer arg = do
  bid <- ask
  BatchT $ lift $ perform (custom (CustomBatchId bid) <> options) producer arg

newBatch :: ToJSON arg => Producer -> BatchOptions arg -> IO BatchId
newBatch producer options = do
  result <- commandByteString
    (producerClient producer)
    "BATCH NEW"
    [encode options]
  case result of
    Left err -> throwString $ "TODO: " <> err
    Right Nothing -> throwString "TODO"
    Right (Just bs) -> pure $ BatchId $ decodeUtf8 $ BSL.toStrict bs

commitBatch :: Producer -> BatchId -> IO ()
commitBatch producer (BatchId bid) = command_
  (producerClient producer)
  "BATCH COMMIT"
  [BSL.fromStrict $ encodeUtf8 bid]
