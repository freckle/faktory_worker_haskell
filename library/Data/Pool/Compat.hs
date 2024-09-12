{-# LANGUAGE CPP #-}

module Data.Pool.Compat
  ( module Data.Pool
  , createPool
  ) where

import Prelude

import Data.Pool hiding (createPool)
#if MIN_VERSION_resource_pool(0,3,0)
#else
import Control.Concurrent (getNumCapabilities)
import qualified Data.Pool as Pool
#endif

createPool
  :: IO a
  -> (a -> IO ())
  -> Double
  -> Int
  -> IO (Pool a)
createPool create destroy timeout size = do
#if MIN_VERSION_resource_pool(0,3,0)
  newPool $ defaultPoolConfig create destroy timeout size
#else
  -- Re-implement instead of using the deprecated compatibility function, so
  -- that we can get a consistent numStripes and size behavior.
  numStripes <- getNumCapabilities
  Pool.createPool create destroy numStripes (realToFrac timeout) size
#endif
