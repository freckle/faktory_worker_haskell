{-# LANGUAGE ImplicitParams #-}
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
import GHC.Stack.Types (HasCallStack)

newtype FaktoryClientException = FaktoryClientException StringException
  deriving (Show)
instance Exception FaktoryClientException

throwClientException :: (MonadThrow m, HasCallStack) => String -> m a
throwClientException s = throwM (FaktoryClientException (StringException s ?callStack))

threadDelaySeconds :: Int -> IO ()
threadDelaySeconds n = threadDelay $ n * 1000000

forkIOWithThrowToParent :: IO () -> IO ThreadId
forkIOWithThrowToParent action = do
  parent <- myThreadId
  forkIO $ action `X.catchAny` \err -> throwTo parent err

fromRightThrows :: MonadThrow m => Either String a -> m a
fromRightThrows = either throwClientException pure
