module Main (main) where

import Prelude

import Data.Aeson
import Faktory.Worker
import GHC.Generics
import UnliftIO.Exception (throwString)

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
