module FDNS.Parsers.Internal where

import Data.Bits
import Data.Word                            (Word8, Word16, Word32)
import Data.Text as T                       (pack, unpack)
import Data.Char                            (chr)
import Data.Text.Encoding                   (encodeUtf8)
import Data.ByteString.Builder              (toLazyByteString, word16BE, word32BE)
import qualified Data.ByteString as BS      (ByteString, unpack, index, length)
import qualified Data.ByteString.Lazy as L  (unpack)
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
  -- | byte > '\ETX' && byte < '\LF'   = domain
  | byte >= '\SOH' && byte <= '?'     = domain ++ "."
  | otherwise                       = domain ++ [byte]

transformQName' :: String -> String -> String
transformQName' domain label = domain ++ [chr (length label)] ++ label

-- TODO Add support for Sequence with pointers
compressionFormat :: BS.ByteString -> COMPRESSION_FORMAT
compressionFormat bytes = if testBit word 6 && testBit word 7 then POINTER else SEQUENCE
  where word = case indexMaybe bytes 0 of
                  (Just x)  -> x
                  Nothing   -> 0

getName :: BS.ByteString -> String
getName bytes = case compressionFormat bytes of
                  POINTER -> "TODO"
                  SEQUENCE -> C.foldl transformQName "" bytes

combineWords :: [Word8] -> Word16
combineWords [] = 0
combineWords (x:xs) = (fromIntegral x `shiftL` 8) + fromIntegral (head xs)

combineWords' :: (Word8, Word8, Word8, Word8) -> Word32
combineWords' (b1, b2, b3, b4) = (fromIntegral b1 `shiftL` 24) +
                                (fromIntegral b2 `shiftL` 16) +
                                (fromIntegral b3 `shiftL` 8) +
                                fromIntegral b4

encodeWord16 :: Word16 -> [Word8]
encodeWord16 = L.unpack . toLazyByteString . word16BE

encodeWord32 :: Word32 -> [Word8]
encodeWord32 = L.unpack . toLazyByteString . word32BE

utf8ToBytes :: String -> [Word8]
utf8ToBytes = BS.unpack . encodeUtf8 . T.pack

indexMaybe :: BS.ByteString -> Int -> Maybe Word8
indexMaybe bs n
    | n < 0                           = Nothing
    | n >= BS.length bs  = Nothing
    | otherwise                       = Just (BS.index bs n)
