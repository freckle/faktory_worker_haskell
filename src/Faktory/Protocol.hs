-- | Modified version of @"Database.Redis.Protocol"@
--
-- <https://github.com/informatikr/hedis/blob/master/src/Database/Redis/Protocol.hs>
--
-- Faktory takes a lot of inspiration from Redis, so the connection and
-- protocol-related code translated well with minor simplifications.
--
module Faktory.Protocol
  ( readReply
  , Reply(..)
  , reply
  ) where

import Prelude hiding (error)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Read as Text
import Scanner (Scanner)
import qualified Scanner

data Reply
  = SingleLine ByteString
  | Error ByteString
  | Bulk (Maybe ByteString)

readReply :: IO ByteString -> IO (Either String (Maybe ByteString))
readReply getMore =
  fromScanResult <$> Scanner.scanWith getMore reply BS8.empty

 where
  fromScanResult = \case
    Scanner.Fail _ msg -> Left msg
    Scanner.More _ -> Left "Unexpected More"
    Scanner.Done _ (SingleLine bs) -> Right $ Just bs
    Scanner.Done _ (Error bs) -> Left $ BS8.unpack bs
    Scanner.Done _ (Bulk mByteString) -> Right mByteString

{-# INLINE reply #-}
reply :: Scanner Reply
reply = do
  c <- Scanner.anyChar8
  case c of
    '+' -> string
    '-' -> error
    '$' -> bulk
    _ -> fail "Unknown reply type"

{-# INLINE string #-}
string :: Scanner Reply
string = SingleLine <$> line

{-# INLINE error #-}
error :: Scanner Reply
error = Error <$> line

{-# INLINE bulk #-}
bulk :: Scanner Reply
bulk = Bulk <$> do
  len <- integral
  if len < 0
    then return Nothing
    else Just <$> Scanner.take len <* eol

{-# INLINE integral #-}
integral :: Integral i => Scanner i
integral = do
  str <- line
  case Text.signed Text.decimal (Text.decodeUtf8 str) of
    Left err -> fail (show err)
    Right (l, _) -> return l

{-# INLINE line #-}
line :: Scanner ByteString
line = Scanner.takeWhileChar8 (/= '\r') <* eol

{-# INLINE eol #-}
eol :: Scanner ()
eol = do
  Scanner.char8 '\r'
  Scanner.char8 '\n'
