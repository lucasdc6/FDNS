module FDNS.Parsers.Internal where

import Data.Bits
import Data.Word
import Data.ByteString hiding (head, foldl)
import qualified Data.ByteString.Char8 as C (splitAt, foldl, span, tail)
import FDNS.Types

getQR :: Word8 -> Bool
getQR byte = testBit byte 7

-- Get operation code from the byte
-- Mask set to
-- 0111 1000
getOpCode :: Word8 -> OPCODE
getOpCode byte = idToOpCode (byte .&. 120)

getAA :: Word8 -> Bool
getAA byte = testBit byte 2

getTC :: Word8 -> Bool
getTC byte = testBit byte 1

getRD :: Word8 -> Bool
getRD byte = testBit byte 0

getRCode :: Word8 -> RCODE
getRCode byte = idToRCode (byte .&. 15)

getQType :: [Word8] -> QTYPE
getQType words = case idToQType (combineWords words) of
                  (Just x)  -> x
                  Nothing   -> NULL


getQClass :: [Word8] -> QCLASS
getQClass words = case idToQClass (combineWords words) of
                      (Just x)  -> x
                      Nothing   -> IN

transformQName :: String -> Char -> String
transformQName domain byte
  | byte == '\ACK' = domain
  | byte == '\ETX' = domain ++ "."
  | otherwise      = domain ++ [byte]

-- TODO Add support for Sequence with pointers
compressionFormat :: ByteString -> COMPRESSION_FORMAT
compressionFormat bytes = if testBit word 6 && testBit word 7 then POINTER else SEQUENCE
  where word = index bytes 0

getName :: ByteString -> String
getName bytes = case compressionFormat bytes of
                  POINTER -> "TODO"
                  SEQUENCE -> C.foldl transformQName "" bytes

combineWords :: [Word8] -> Word16
combineWords [] = 0
combineWords (x:xs) = (fromIntegral x `Data.Bits.shiftL` 8) + fromIntegral (head xs)
