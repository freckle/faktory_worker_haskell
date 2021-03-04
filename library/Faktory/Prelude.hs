module Faktory.Prelude
  ( module X
  , module Faktory.Prelude
  )
where

import Prelude as X

import Control.Concurrent (ThreadId, forkIO, myThreadId, threadDelay)
import Control.Monad as X
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable as X
import Data.Text as X (Text, pack, unpack)
import Data.Traversable as X
import UnliftIO.Exception as X

threadDelaySeconds :: Int -> IO ()
threadDelaySeconds n = threadDelay $ n * 1000000

forkIOWithThrowToParent :: IO () -> IO ThreadId
forkIOWithThrowToParent action = do
  parent <- myThreadId
  forkIO $ action `X.catchAny` \err -> throwTo parent err

fromRightThrows :: MonadIO m => Either String a -> m a
fromRightThrows = either throwString pure
