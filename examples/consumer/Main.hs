module Main (main) where

import Prelude

import Control.Exception.Safe
import Data.Aeson
import Faktory.Settings
import Faktory.Worker
import GHC.Generics

-- | Must match examples/producer
newtype Job = Job { jobMessage :: String }
  deriving Generic
instance FromJSON Job

main :: IO ()
main = do
  putStrLn "Starting consumer loop"
  runWorker defaultSettings defaultQueue $ \job -> do
    let message = jobMessage job

    if message == "BOOM"
      then throwString "Producer exception: BOOM"
      else putStrLn message
