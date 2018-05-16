module Main (main) where

import Prelude

import Data.Aeson
import Data.Semigroup ((<>))
import Faktory.Client
import Faktory.Settings
import GHC.Generics
import System.Environment (getArgs)

-- | Must match examples/consumer
newtype Job = Job { jobMessage :: String }
  deriving Generic
instance ToJSON Job

main :: IO ()
main = do
  settings <- defaultSettings <$> defaultAddrInfo
  withClient settings $ \client -> do
    args <- getArgs
    jobId <- pushJob client defaultQueue Job
      { jobMessage = unwords args
      }

    putStrLn $ "Pushed job: " <> show jobId
