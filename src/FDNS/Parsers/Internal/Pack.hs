module FDNS.Parsers.Internal.Pack where

import Data.Word                            (Word8, Word16)
import Data.Char                            (chr, ord)
import Text.Read                            (readMaybe)
import Data.List.Split                      (splitOn, chunksOf)
import qualified Data.ByteString as BS      (ByteString, empty, pack)
import qualified Data.ByteString.UTF8 as U  (fromString)

import FDNS.Types
import FDNS.Parsers.Internal.Utils

packQName :: String -> String -> String
packQName domain label = domain ++ [chr (length label)] ++ label

packRData :: QTYPE -> String -> BS.ByteString
packRData A    rdata  = packArdata rdata
packRData AAAA rdata  = packAAAArdata rdata
packRData MX   rdata  = packMXrdata rdata
packRData _    _      = BS.empty

packArdata :: String -> BS.ByteString
packArdata rdata = BS.pack (map packWord8 (splitOn "." rdata))

packAAAArdata :: String -> BS.ByteString
packAAAArdata rdata =
  let bytes = (splitOn ":" rdata) >>= (\x -> chunksOf 2 x)
  in BS.pack (map (\x -> packWord8 ("0x" ++ x)) bytes)

packMXrdata :: String -> BS.ByteString
packMXrdata rdata =
  let (preference:(domain:xs)) = splitOn " " rdata
  in case readMaybe preference :: Maybe Word16 of
    (Just x)  -> let labels = drop 1 (splitOn "." domain)
                 in BS.pack ((encodeWord16 x) ++ (map (\x -> fromIntegral (ord x) :: Word8) ((foldl packQName "" labels) ++ "\NUL")))
    Nothing   -> BS.empty
