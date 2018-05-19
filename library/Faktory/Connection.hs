module Faktory.Connection
  ( Connection(..)
  , defaultConnection
  , envConnection
  , connect
  ) where

import Faktory.Prelude

import Data.Default.Class (def)
import Data.Maybe (fromMaybe)
import Data.Void
import qualified Network.Connection as Con
import Network.Socket (HostName, PortNumber)
import System.Environment (lookupEnv)
import Text.Megaparsec
import Text.Megaparsec.Char

data Connection = Connection
  { connectionTls :: Bool
  , connectionPassword :: Maybe String
  , connectionHostName :: HostName
  , connectionPort :: PortNumber
  }
  deriving (Eq, Show)

defaultConnection :: Connection
defaultConnection = Connection
  { connectionTls = False
  , connectionPassword = Nothing
  , connectionHostName = "localhost"
  , connectionPort = 7419
  }

-- | Parse a @'Connection'@ from environment variables
--
-- > FAKTORY_PROVIDER=FAKTORY_URL
-- > FAKTORY_URL=tcp://:my-password@localhost:7419
--
-- Supported format is @tcp(+tls):\/\/(:password@)host:port@.
--
-- See <https://github.com/contribsys/faktory/wiki/Worker-Lifecycle#url-configuration>.
--
envConnection :: IO Connection
envConnection = do
  providerString <- fromMaybe "FAKTORY_URL" <$> lookupEnv "FAKTORY_PROVIDER"
  provider <- parseThrow parseProvider "FAKTORY_PROVIDER" providerString
  connectionString <- fromMaybe "tcp://localhost:7419" <$> lookupEnv provider
  parseThrow parseConnection provider connectionString

-- | Connect to the given @'Connection'@ as a @'Socket'@
connect :: Connection -> IO Con.Connection
connect Connection{..} =
  bracketOnError open Con.connectionClose pure
 where
  open = do
    ctx <- Con.initConnectionContext
    Con.connectTo ctx $ Con.ConnectionParams
      { Con.connectionHostname  = connectionHostName
      , Con.connectionPort      = connectionPort
      , Con.connectionUseSecure = if connectionTls then Just def else Nothing
      , Con.connectionUseSocks  = Nothing
      }

type Parser = Parsec Void String

parseThrow :: Parser a -> String -> String -> IO a
parseThrow parser name value = either err pure $ parse parser name value
 where
  err ex = throwIO . userError $ unlines
    [ ""
    , "\"" <> value <> "\" is an invalid value for " <> name <> ":"
    , parseErrorPretty ex
    ]

parseProvider :: Parser String
parseProvider = some (upperChar <|> char '_')
  <?> "an environment variable name"

parseConnection :: Parser Connection
parseConnection = go <?> "tcp(+tls)://(:<password>@)<host>:<port>"
 where
  go = Connection
    <$> (False <$ string "tcp://" <|> True <$ string "tcp+tls://")
    <*> optional (char ':' *> manyTill anyChar (char '@'))
    <*> manyTill anyChar (char ':')
    <*> (read <$> some digitChar)
