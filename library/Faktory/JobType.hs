module Faktory.JobType
  ( HasJobType(..)
  ) where

import Faktory.Prelude

class HasJobType a where
  jobTypeName :: a -> Text
