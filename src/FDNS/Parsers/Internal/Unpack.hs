module FDNS.Parsers.Internal.Unpack where

import Numeric                              (showHex)
import Data.List                            (intersperse, intercalate)
import Data.Char                            (chr)
import Data.Maybe                           (fromMaybe)
import Data.List.Split                      (chunksOf)
import qualified Data.ByteString as BS      (ByteString, unpack, take, length)
import qualified Data.ByteString.Char8 as C (foldl)
import FDNS.Types
import FDNS.Parsers.Internal.Utils

getName :: BS.ByteString -> String
getName bytes = case compressionFormat bytes of
                  POINTER -> "TODO"
                  SEQUENCE -> C.foldl unpackQName "" bytes

unpackQName :: String -> Char -> String
unpackQName domain byte
  | byte >= '\NUL' && byte <= '?'     = domain ++ "."
  | otherwise                         = domain ++ [byte]


unpackRdata :: QTYPE -> BS.ByteString -> String
unpackRdata A    bytes = fromMaybe "" (unpackArdata bytes)
unpackRdata AAAA bytes = fromMaybe "" (unpackAAAArdata bytes)
unpackRdata MX   bytes = fromMaybe "" (unpackMXrdata bytes)
unpackRdata _ _        = ""

unpackArdata :: BS.ByteString -> Maybe String
unpackArdata bytes =
  if BS.length bytes >= 4
    then Just(intersperse '.' rdata)
    else Nothing
  where rdataBytes = BS.unpack (BS.take 4 bytes)
        rdata = map (\byte -> chr (fromIntegral byte)) rdataBytes

unpackAAAArdata :: BS.ByteString -> Maybe String
unpackAAAArdata bytes =
  if BS.length bytes >= 16
    then Just(rdata)
    else Nothing
  where rdataBytes = BS.unpack (BS.take 16 bytes)
        rdataRaw = foldr (\x hex -> if x <= 15 then "0" ++ (showHex x hex) else showHex x hex ) "" rdataBytes
        rdata = intercalate ":" (chunksOf 4 rdataRaw)

unpackMXrdata :: BS.ByteString -> Maybe String
unpackMXrdata bytes = Just ""
