module Faktory.Connection
  ( Connection(..)
  , defaultConnection
  , envConnection
  , connect
  ) where

import Faktory.Prelude

import Data.Maybe (fromMaybe)
import Data.Void
import Network.Socket (HostName, PortNumber)
import qualified Network.Socket as NS
import System.Environment (lookupEnv)
import Text.Megaparsec
import Text.Megaparsec.Char

data Connection = Connection
  { connectionHostName :: HostName
  , connectionPort :: PortNumber
  }
  deriving (Eq, Show)

defaultConnection :: Connection
defaultConnection = Connection
  { connectionHostName = "localhost"
  , connectionPort = 7419
  }

-- | Parse a @'Connection'@ from environment variables
--
-- > FAKTORY_PROVIDER=FAKTORY_URL
-- > FAKTORY_URL=tcp://:password@host:port
--
envConnection :: IO Connection
envConnection = do
  providerString <- fromMaybe "FAKTORY_URL" <$> lookupEnv "FAKTORY_PROVIDER"
  provider <- parseThrow parseProvider "FAKTORY_PROVIDER" providerString
  connectionString <- fromMaybe "tcp://localhost:7419" <$> lookupEnv provider
  parseThrow parseConnection provider connectionString

-- | Connect to the given @'Connection'@ as a @'Socket'@
connect :: Connection -> IO NS.Socket
connect connection =
  bracketOnError open NS.close pure
 where
  open = do
    addr <- fetchSocketAddressInfo connection
    sock <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
    NS.connect sock $ NS.addrAddress addr
    pure sock

fetchSocketAddressInfo :: Connection -> IO NS.AddrInfo
fetchSocketAddressInfo Connection{..} = do
  let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
  head <$> NS.getAddrInfo (Just hints) (Just connectionHostName) (Just $ show connectionPort)

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
  go = do
    -- TODO: connectionTls :: Bool, connectionPassword :: Maybe String
    void $ string "tcp://" <|> string "tcp+tls://"
    void $ optional $ char ':' *> manyTill anyChar (char '@')

    Connection
      <$> manyTill anyChar (char ':')
      <*> (read <$> some digitChar)
