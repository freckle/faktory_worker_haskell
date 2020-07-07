module Faktory.Producer
  ( Producer
  , newProducer
  , newProducerEnv
  , closeProducer
  , pushJob
  , flush
  )
where

import Faktory.Prelude

import Data.Aeson
import Faktory.Client
import Faktory.Settings
import GHC.Stack

newtype Producer = Producer
  { producerClient :: Client
  }

newProducer :: Settings -> IO Producer
newProducer settings = Producer <$> newClient settings Nothing

newProducerEnv :: IO Producer
newProducerEnv = newProducer =<< envSettings

closeProducer :: Producer -> IO ()
closeProducer = closeClient . producerClient

-- | Push a Job to the Server
pushJob :: (HasCallStack, ToJSON a) => Producer -> a -> IO ()
pushJob producer job = commandOK (producerClient producer) "PUSH" [encode job]

-- | Clear all job data in the Faktory server
--
-- Use with caution!
--
flush :: HasCallStack => Producer -> IO ()
flush producer = commandOK (producerClient producer) "FLUSH" []
