module Faktory.Connection
  ( ConnectionInfo(..)
  , Namespace(..)
  , defaultConnectionInfo
  , envConnectionInfo
  , connect
  ) where

import Faktory.Prelude

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Void
import Network.Connection
import Network.Socket hiding (connect)
import qualified Network.Socket as S
import System.Environment (lookupEnv)
import Text.Megaparsec
  (Parsec, anySingle, errorBundlePretty, manyTill, optional, parse, some, (<?>))
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
defaultConnectionInfo = ConnectionInfo
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
--
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
    connectTo' ctx $ ConnectionParams
      { connectionHostname = connectionInfoHostName
      , connectionPort = connectionInfoPort
      , connectionUseSecure = if connectionInfoTls
        then Just TLSSettingsSimple
          { settingDisableCertificateValidation = False
          , settingDisableSession = False
          , settingUseServerName = False
          }
        else Nothing
      , connectionUseSocks = Nothing
      }

  connectTo' :: ConnectionContext -> ConnectionParams -> IO Connection
  connectTo' cg cParams = bracketOnError
    (resolve' (connectionHostname cParams) (connectionPort cParams))
    (close . fst)
    (\(h, _) -> connectFromSocket cg h cParams)

  -- This is copy and pasted from the connectTo implement. The only change is
  -- adding a keep alive socket option.
  -- see: https://hackage.haskell.org/package/connection-0.3.1/docs/src/Network.Connection.html#connectTo
  resolve' :: String -> PortNumber -> IO (Socket, SockAddr)
  resolve' host port = do
    let
      hints =
        defaultHints { addrFlags = [AI_ADDRCONFIG], addrSocketType = Stream }
    addrs <- getAddrInfo (Just hints) (Just host) (Just $ show port)
    firstSuccessful $ map tryToConnect addrs
   where
    tryToConnect addr = bracketOnError
      (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
      close
      (\sock -> do
        setSocketOption sock KeepAlive 1
        S.connect sock (addrAddress addr)
        return (sock, addrAddress addr)
      )
    firstSuccessful = go []
     where
      go :: [IOException] -> [IO a] -> IO a
      go [] [] = throwIO $ HostNotResolved host
      go l@(_ : _) [] = throwIO $ HostCannotConnect host l
      go acc (act : followingActs) = do
        er <- try act
        case er of
          Left err -> go (err : acc) followingActs
          Right r -> return r

type Parser = Parsec Void String

parseThrow :: Parser a -> String -> String -> IO a
parseThrow parser name value = either err pure $ parse parser name value
 where
  err ex = throwIO . userError $ unlines
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
