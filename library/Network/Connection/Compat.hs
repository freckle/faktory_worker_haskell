-- | Copy of 'Network.Connection.connectTo' with 'KeepAlive'
--
-- <https://hackage.haskell.org/package/connection-0.3.1/docs/src/Network.Connection.html#connectTo>
module Network.Connection.Compat
  ( connectTo
  , module Network.Connection
  ) where

import Prelude

import Control.Exception (IOException, bracketOnError, throwIO, try)
import Network.Connection hiding (connectTo)
import Network.Socket
import qualified Network.Socket as S

connectTo :: ConnectionContext -> ConnectionParams -> IO Connection
connectTo cg cParams =
  bracketOnError
    (resolve (connectionHostname cParams) (connectionPort cParams))
    (close . fst)
    ( \(h, _) ->
        connectFromSocket cg h cParams
    )

resolve :: String -> PortNumber -> IO (Socket, SockAddr)
resolve host port = do
  let hints = defaultHints {addrFlags = [AI_ADDRCONFIG], addrSocketType = Stream}
  addrs <- getAddrInfo (Just hints) (Just host) (Just $ show port)
  firstSuccessful $ map tryToConnect addrs
 where
  tryToConnect addr =
    bracketOnError
      (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
      close
      ( \sock -> do
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
