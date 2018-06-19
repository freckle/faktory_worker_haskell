module Main (main) where

import Prelude

import Control.Exception.Safe
import Data.Aeson
import Data.Semigroup ((<>))
import Faktory.Client
import Faktory.Job (perform)
import Faktory.Settings
import GHC.Generics
import System.Environment (getArgs)

-- | Must match examples/consumer
newtype Job = Job { jobMessage :: String }
  deriving Generic
instance ToJSON Job

main :: IO ()
main = do
  settings <- envSettings
  bracket (newClient settings Nothing) closeClient $ \client -> do
    args <- getArgs
    jobId <- perform client defaultQueue Job
      { jobMessage = unwords args
      }

    putStrLn $ "Pushed job: " <> show jobId
