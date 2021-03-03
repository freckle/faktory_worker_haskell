-- | Wrapper for the schema-less 'Value' used for @custom@ in Job payloads
--
-- This type's 'Semigroup' will merge two Objects. It is right-biased, as we are
-- generally throughout this library, so called /last-wins/ semantics.
--
module Faktory.Job.Custom
    ( Custom
    , toCustom
    ) where

import Faktory.Prelude

import Data.Aeson
import qualified Data.HashMap.Strict as HashMap

newtype Custom = Custom Value
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToJSON)

toCustom :: ToJSON a => a -> Custom
toCustom = Custom . toJSON

instance Semigroup Custom where
  (Custom (Object a)) <> (Custom (Object b)) =
    Custom $ Object $ HashMap.union b a
  _ <> b = b
