module FDNS.Parsers.Internal.Pack where

import Data.Word                            (Word8, Word16)
import Data.Char                            (chr)
import Text.Read                            (readMaybe)
import Data.List.Split                      (splitOn, chunksOf)
import qualified Data.ByteString as BS      (ByteString, empty, pack)

import FDNS.Types
import FDNS.Parsers.Internal.Utils

packQName :: String -> String -> String
packQName domain label = domain ++ [chr (length label)] ++ label

packRData :: QTYPE -> String -> BS.ByteString
packRData A    rdata  = packArdata rdata
packRData AAAA rdata  = packAAAArdata rdata
packRData MX   rdata  = packMXrdata rdata
packRData _ _         = BS.empty

packArdata :: String -> BS.ByteString
packArdata rdata = BS.pack (map packWord8 (splitOn "." rdata))

packAAAArdata :: String -> BS.ByteString
packAAAArdata rdata =
  let bytes = (splitOn ":" rdata) >>= (\x -> chunksOf 2 x)
  in BS.pack (map (\x -> packWord8 ("0x" ++ x)) bytes)

packMXrdata :: String -> BS.ByteString
packMXrdata rdata =
  let (preference:xs) = splitOn " " rdata
  in case readMaybe preference :: Maybe Word16 of
    (Just x)  -> BS.pack (encodeWord16 x)
    Nothing   -> BS.empty
