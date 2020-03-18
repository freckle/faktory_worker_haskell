module Faktory.Prelude
  ( module X
  , module Faktory.Prelude
  )
where

import Prelude as X

import Control.Concurrent (ThreadId, forkIO, myThreadId, threadDelay)
import Control.Exception.Safe as X
import Control.Monad as X
import Data.Foldable as X
import Data.Text as X (Text, pack, unpack)
import Data.Traversable as X

threadDelaySeconds :: Int -> IO ()
threadDelaySeconds n = threadDelay $ n * 1000000

forkIOWithThrowToParent :: IO () -> IO ThreadId
forkIOWithThrowToParent action = do
  parent <- myThreadId
  forkIO $ action `X.catchAny` \err -> throwTo parent err
