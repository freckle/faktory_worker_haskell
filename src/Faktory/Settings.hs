{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Faktory.Settings
  ( Settings(..)
  , defaultSettings
  , Queue
  , queueArg
  , defaultQueue
  ) where

import Data.Aeson
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Semigroup ((<>))
import Data.String
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network
import System.IO (hPutStrLn, stderr)

data Settings = Settings
  { settingsHost :: HostName
  , settingsPort :: PortID
  , settingsLogDebug :: String -> IO ()
  , settingsLogError :: String -> IO ()
  }

defaultSettings :: Settings
defaultSettings = Settings
  { settingsHost = "localhost"
  , settingsPort = PortNumber 7419
  , settingsLogDebug = \_msg -> pure ()
  , settingsLogError = hPutStrLn stderr . ("[ERROR]: " <>)
  }

newtype Queue = Queue Text
  deriving (IsString, FromJSON, ToJSON)

queueArg :: Queue -> ByteString
queueArg (Queue q) = fromStrict $ encodeUtf8 q

defaultQueue :: Queue
defaultQueue = "default"
