module Faktory.Connection
  ( ConnectionInfo (..)
  , Namespace (..)
  , defaultConnectionInfo
  , envConnectionInfo
  , connect
  ) where

import Faktory.Prelude

import Control.Applicative ((<|>))
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Void
import Network.Connection.Compat
import Network.Socket (HostName, PortNumber)
import System.Environment (lookupEnv)
import Text.Megaparsec
  ( Parsec
  , anySingle
  , errorBundlePretty
  , manyTill
  , optional
  , parse
  , some
  , (<?>)
  )
import Text.Megaparsec.Char (char, digitChar, string, upperChar)

newtype Namespace = Namespace Text
  deriving newtype (Eq, Show)

data ConnectionInfo = ConnectionInfo
  { connectionInfoTls :: Bool
  , connectionInfoPassword :: Maybe String
  , connectionInfoHostName :: HostName
  , connectionInfoPort :: PortNumber
  , connectionInfoNamespace :: Namespace
  }
  deriving stock (Eq, Show)

defaultConnectionInfo :: ConnectionInfo
defaultConnectionInfo =
  ConnectionInfo
    { connectionInfoTls = False
    , connectionInfoPassword = Nothing
    , connectionInfoHostName = "localhost"
    , connectionInfoPort = 7419
    , connectionInfoNamespace = Namespace ""
    }

-- | Parse a @'Connection'@ from environment variables
--
-- > FAKTORY_PROVIDER=FAKTORY_URL
-- > FAKTORY_URL=tcp://:my-password@localhost:7419
--
-- Supported format is @tcp(+tls):\/\/(:password@)host:port(/namespace)@.
--
-- See <https://github.com/contribsys/faktory/wiki/Worker-Lifecycle#url-configuration>.
envConnectionInfo :: IO ConnectionInfo
envConnectionInfo = do
  providerString <- fromMaybe "FAKTORY_URL" <$> lookupEnv "FAKTORY_PROVIDER"
  provider <- parseThrow parseProvider "FAKTORY_PROVIDER" providerString
  connectionString <- fromMaybe "tcp://localhost:7419" <$> lookupEnv provider
  parseThrow parseConnection provider connectionString

-- | Connect to the given @'Connection'@ as a @'Socket'@
connect :: ConnectionInfo -> IO Connection
connect ConnectionInfo {..} = bracketOnError open connectionClose pure
 where
  open = do
    ctx <- initConnectionContext
    connectTo ctx $
      ConnectionParams
        { connectionHostname = connectionInfoHostName
        , connectionPort = connectionInfoPort
        , connectionUseSecure =
            if connectionInfoTls
              then Just def
              else Nothing
        , connectionUseSocks = Nothing
        }

type Parser = Parsec Void String

parseThrow :: Parser a -> String -> String -> IO a
parseThrow parser name value = either err pure $ parse parser name value
 where
  err ex =
    throwIO . userError $
      unlines
        [ ""
        , "\"" <> value <> "\" is an invalid value for " <> name <> ":"
        , errorBundlePretty ex
        ]

parseProvider :: Parser String
parseProvider =
  some (upperChar <|> char '_') <?> "an environment variable name"

parseConnection :: Parser ConnectionInfo
parseConnection = go <?> "tcp(+tls)://(:<password>@)<host>:<port>(/namespace)"
 where
  go =
    ConnectionInfo
      <$> (False <$ string "tcp://" <|> True <$ string "tcp+tls://")
      <*> optional (char ':' *> manyTill anySingle (char '@'))
      <*> manyTill anySingle (char ':')
      <*> (read <$> some digitChar)
      <*> (toNamespace <$> optional (char '/' *> some anySingle))
  toNamespace = Namespace . maybe "" pack
