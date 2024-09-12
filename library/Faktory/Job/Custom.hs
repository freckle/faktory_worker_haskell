-- | Wrapper for the schema-less 'Value' used for @custom@ in Job payloads
--
-- This type's 'Semigroup' will merge two Objects. It is right-biased, as we are
-- generally throughout this library, so called /last-wins/ semantics.
module Faktory.Job.Custom
  ( Custom
  , toCustom
  , fromCustom
  ) where

import Faktory.Prelude

import Data.Aeson

newtype Custom = Custom Value
  deriving stock (Eq, Show)
  deriving newtype (FromJSON, ToJSON)

toCustom :: ToJSON a => a -> Custom
toCustom = Custom . toJSON

-- | Read a 'Custom' value to a type using 'FromJSON'
fromCustom :: FromJSON a => Custom -> Either String a
fromCustom (Custom v) = case fromJSON v of
  Error e -> Left e
  Success a -> Right a

instance Semigroup Custom where
  (Custom (Object a)) <> (Custom (Object b)) = Custom $ Object $ b <> a
  _ <> b = b
