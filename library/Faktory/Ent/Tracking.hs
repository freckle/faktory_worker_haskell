-- | Support for the @TRACK@ command (Enterprise only)
--
-- <https://github.com/contribsys/faktory/wiki/Ent-Tracking>
--
module Faktory.Ent.Tracking
  ( CustomTrack(..)
  , tracked
  , trackPerform

  , JobDetails(..)
  , JobState(..)
  , trackGet
  , trackGetHush

  , SetJobDetails(..)
  , trackSet
  ) where

import Faktory.Prelude

import Control.Error.Util (hush)
import Data.Aeson
import Data.Aeson.Casing
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Maybe (fromMaybe)
import Data.Time (UTCTime)
import Faktory.Client (commandJSON, commandOK)
import Faktory.Job (JobId, JobOptions, custom, perform)
import Faktory.JobState (JobState(..))
import Faktory.Producer
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

newtype CustomTrack = CustomTrack
  { track :: Int
  }
  deriving stock Generic
  deriving anyclass ToJSON

tracked :: JobOptions
tracked = custom (CustomTrack 1)

-- | 'perform', but adding @{ custom: { track: 1 } }@
--
-- Equivalent to:
--
-- @
-- 'perform' ('custom' $ 'CustomTrack' 1)
-- @
--
trackPerform
  :: (HasCallStack, ToJSON arg) => JobOptions -> Producer -> arg -> IO JobId
trackPerform options = perform (options <> custom (CustomTrack 1))
{-# DEPRECATED trackPerform "Use ‘perform (options <> tracked)’ instead" #-}

data JobDetails = JobDetails
  { jdJid :: JobId
  , jdPercent :: Maybe Int
  , jdDesc :: Maybe Text
  , jdState :: JobState
  , jdUpdatedAt :: Maybe UTCTime
  }
  deriving stock Generic

instance FromJSON JobDetails where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

unknownJobDetails :: JobId -> JobDetails
unknownJobDetails jid = JobDetails
  { jdJid = jid
  , jdPercent = Nothing
  , jdDesc = Nothing
  , jdState = JobStateUnknown
  , jdUpdatedAt = Nothing
  }

data SetJobDetails = SetJobDetails
  { sjdJid :: JobId
  , sjdPercent :: Maybe Int
  , sjdDesc :: Maybe Text
  , sjdReserveUntil :: Maybe UTCTime
  }
  deriving stock Generic

instance ToJSON SetJobDetails where
  toJSON = genericToJSON $ aesonPrefix snakeCase
  toEncoding = genericToEncoding $ aesonPrefix snakeCase

trackGet :: Producer -> JobId -> IO (Either String (Maybe JobDetails))
trackGet producer jid =
  commandJSON (producerClient producer) "TRACK GET" [BSL8.pack jid]

-- | 'trackGet' but mask any failures to 'JobStateUnknown'
trackGetHush :: Producer -> JobId -> IO JobDetails
trackGetHush producer jid =
  fromMaybe (unknownJobDetails jid) . join . hush <$> trackGet producer jid

trackSet :: HasCallStack => Producer -> SetJobDetails -> IO ()
trackSet producer details =
  commandOK (producerClient producer) "TRACK SET" [encode details]
