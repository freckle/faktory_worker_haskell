module Faktory.Settings.Queue
  ( Queue (..)
  , namespaceQueue
  , queueArg
  , defaultQueue
  , Namespace (..)
  ) where

import Faktory.Prelude

import Data.Aeson
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.String
import Data.Text.Encoding (encodeUtf8)
import Faktory.Connection

newtype Queue = Queue Text
  deriving stock (Eq, Show)
  deriving newtype (IsString, FromJSON, ToJSON)

namespaceQueue :: Namespace -> Queue -> Queue
namespaceQueue (Namespace n) (Queue q) = Queue $ mappend n q

queueArg :: Queue -> ByteString
queueArg (Queue q) = fromStrict $ encodeUtf8 q

defaultQueue :: Queue
defaultQueue = "default"
