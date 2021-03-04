module Main (main) where

import Prelude

import Conduit hiding (Producer)
import Data.Aeson
import Data.Foldable (for_)
import Faktory.Ent.Batch
import Faktory.Job (buildJob)
import Faktory.Producer
import GHC.Generics
import System.Environment (getArgs)
import UnliftIO.Exception (bracket)

-- | Must match examples/consumer
newtype Job = Job { jobMessage :: String }
  deriving stock Generic
  deriving anyclass ToJSON

main :: IO ()
main = do
  args <- getArgs
  bracket newProducerEnv closeProducer $ \producer -> do
    onComplete <- liftIO $ buildJob mempty producer $ Job "Done"
    let options = complete onComplete <> description "My Batch"
    runConduit $ yieldMany args .| mapC Job .| sinkBatchT options producer

sinkBatchT
  :: (MonadIO m, ToJSON i) => BatchOptions i -> Producer -> ConduitT i o m ()
sinkBatchT options producer = runBatchT options producer $ do
  let
    loop = do
      mJob <- lift await

      for_ mJob $ \job -> do
        jobId <- batchPerform mempty producer job
        liftIO $ putStrLn $ "Enqueued in Batch: " <> show jobId
        loop

  loop
