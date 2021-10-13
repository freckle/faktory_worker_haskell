module Main (main) where

import Prelude

import Control.Exception.Safe
import Data.Aeson
import Faktory.Job (HasJobType(..), perform)
import Faktory.Producer
import GHC.Generics
import System.Environment (getArgs)

-- | Must match examples/consumer
newtype Job = Job { jobMessage :: String }
  deriving stock Generic
  deriving anyclass ToJSON

instance HasJobType Job where
  jobTypeName _ = "Job"

main :: IO ()
main = bracket newProducerEnv closeProducer $ \producer -> do
  args <- getArgs
  jobId <- perform mempty producer Job { jobMessage = unwords args }

  putStrLn $ "Pushed job: " <> show jobId
