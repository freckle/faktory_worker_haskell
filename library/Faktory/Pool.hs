module Faktory.Pool
  ( FaktoryPool
  , HasFaktoryPool (..)

    -- * Pool Construction
  , Settings
  , PoolSettings
  , newFaktoryPool

    -- * Pool use
  , perform
  , buildJob

    -- * Direct access
  , withProducer
  , takeProducer

    -- * Re-exports
  , module Faktory.Job
  ) where

import Faktory.Prelude

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson (ToJSON)
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Faktory.Job hiding (buildJob, perform)
import qualified Faktory.Job as Job
import Faktory.Producer
import Faktory.Settings (PoolSettings (..), Settings)
import GHC.Stack (HasCallStack)
import Lens.Micro (Lens', (^.))
import UnliftIO (MonadUnliftIO, withRunInIO)

-- |
--
-- @since 1.1.3.0
newtype FaktoryPool = FaktoryPool (Pool Producer)

-- |
--
-- @since 1.1.3.0
class HasFaktoryPool env where
  faktoryPoolL :: Lens' env FaktoryPool

instance HasFaktoryPool FaktoryPool where
  faktoryPoolL = id

-- | Build a 'FaktoryPool' with the given settings
--
-- See 'Settings', 'envSettings', 'PoolSettings', and 'envPoolSettings'.
--
-- @since 1.1.3.0
newFaktoryPool
  :: MonadIO m
  => Settings
  -> PoolSettings
  -> m FaktoryPool
newFaktoryPool settings PoolSettings {..} = do
  liftIO
    . fmap FaktoryPool
    . Pool.newPool
    $ Pool.defaultPoolConfig
      (newProducer settings)
      closeProducer
      (fromIntegral settingsTimeout)
      (fromIntegral settingsSize)

-- | 'Faktory.Job.perform' but using a 'Producer' from the pool
--
-- @since 1.1.3.0
perform
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasFaktoryPool env
     , ToJSON arg
     , HasCallStack
     )
  => JobOptions
  -> arg
  -> m JobId
perform options arg = do
  withProducer $ \producer -> do
    liftIO $ Job.perform options producer arg

-- | 'Faktory.Job.buildJob' but using a 'Producer' from the pool
--
-- @since 1.1.3.0
buildJob
  :: (MonadUnliftIO m, MonadReader env m, HasFaktoryPool env)
  => JobOptions
  -> arg
  -> m (Job arg)
buildJob options arg = do
  withProducer $ \producer -> do
    liftIO $ Job.buildJob options producer arg

-- | Acquire a 'Producer', use it, and return it to the pool
--
-- @since 1.1.3.0
withProducer
  :: (MonadUnliftIO m, MonadReader env m, HasFaktoryPool env)
  => (Producer -> m a)
  -> m a
withProducer f = do
  FaktoryPool p <- asks (^. faktoryPoolL)
  withRunInIO $ \runInIO -> do
    Pool.withResource p $ runInIO . f

-- | Get a 'Producer' from the pool along with an action to return it
--
-- You should prefer 'withProducer' if at all possible. With this function you
-- are responsible to ensure the return action is called (e.g. with 'finally').
--
-- This is only necessary if you are operating in a monad that doesn't have
-- 'MonadUnliftIO' (like 'ConduitT'), so you need to take and return a
-- 'Producer' separately (e.g. with 'bracketP').
--
-- @since 1.1.3.0
takeProducer
  :: (MonadIO m, MonadReader env m, HasFaktoryPool env) => m (Producer, m ())
takeProducer = do
  FaktoryPool p <- asks (^. faktoryPoolL)
  (producer, lp) <- liftIO $ Pool.takeResource p
  pure (producer, liftIO $ Pool.putResource lp producer)
