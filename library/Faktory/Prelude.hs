module Faktory.Prelude
  ( module X
  , module Faktory.Prelude
  )
where

import Prelude as X

import Control.Concurrent (ThreadId, forkFinally, myThreadId, threadDelay)
import Control.Exception.Safe as X
import Control.Monad as X
import Data.Foldable as X
import Data.Semigroup as X ((<>))
import Data.Text as X (Text)
import Data.Traversable as X

threadDelaySeconds :: Int -> IO ()
threadDelaySeconds n = threadDelay $ n * 1000000

forkIOWithThrowToParent :: IO () -> IO ThreadId
forkIOWithThrowToParent action = do
  parent <- myThreadId
  forkFinally action $ \case
    Left err -> throwTo parent err
    Right _ -> pure ()
