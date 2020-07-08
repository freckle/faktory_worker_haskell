module Main (main) where

import Prelude

import Control.Exception.Safe
import Data.Aeson
import Faktory.Worker
import GHC.Generics

-- | Must match examples/producer
newtype Job = Job { jobMessage :: String }
  deriving stock Generic
  deriving anyclass FromJSON

main :: IO ()
main = do
  putStrLn "Starting consumer loop"
  runWorkerEnv $ \job -> do
    let message = jobMessage job

    if message == "BOOM"
      then throwString "Producer exception: BOOM"
      else putStrLn message
