module Faktory.Connection
  ( Connection(..)
  , defaultConnection
  , connect
  ) where

import Faktory.Prelude

import Network.Socket (HostName, PortNumber)
import qualified Network.Socket as NS

data Connection = Connection
  { connectionHostName :: HostName
  , connectionPort :: PortNumber
  }

defaultConnection :: Connection
defaultConnection = Connection
  { connectionHostName = "localhost"
  , connectionPort = 7419
  }

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
