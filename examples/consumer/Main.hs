module Main (main) where

import Prelude

import Control.Exception.Safe
import Control.Monad
import Data.Aeson
import Faktory.Client
import Faktory.Settings
import GHC.Generics

-- | Must match examples/producer
newtype Job = Job { jobMessage :: String }
  deriving Generic
instance FromJSON Job

main :: IO ()
main = withClient defaultSettings $ \client -> do
  putStrLn "Starting consumer loop"
  forever $ withJob client defaultQueue $ \job -> do
    let message = jobMessage job

    if message == "BOOM"
      then throwString "Producer exception: BOOM"
      else putStrLn message
