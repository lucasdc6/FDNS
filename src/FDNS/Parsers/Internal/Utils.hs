module FDNS.Parsers.Internal.Utils where

import Data.Bits                            (shiftL, testBit)
import Data.Word                            (Word8, Word16, Word32)
import Text.Read                            (readMaybe)
import Data.Text as T                       (pack)
import Data.Text.Encoding                   (encodeUtf8)
import Data.ByteString.Builder              (toLazyByteString, word16BE, word32BE)
import qualified Data.ByteString as BS      (ByteString, unpack, index, length)
import qualified Data.ByteString.Lazy as L  (unpack)

import FDNS.Types

-- TODO Add support for Sequence with pointers
compressionFormat :: BS.ByteString -> COMPRESSION_FORMAT
compressionFormat bytes = if testBit word 6 && testBit word 7 then POINTER else SEQUENCE
  where word = case indexMaybe bytes 0 of
                  (Just x)  -> x
                  Nothing   -> 0


combineWords2 :: (Word8, Word8) -> Word16
combineWords2 (b1, b2) = (fromIntegral b1 `shiftL` 8) + fromIntegral b2

combineWords4 :: (Word8, Word8, Word8, Word8) -> Word32
combineWords4 (b1, b2, b3, b4) = (fromIntegral b1 `shiftL` 24) +
                                 (fromIntegral b2 `shiftL` 16) +
                                 (fromIntegral b3 `shiftL` 8) +
                                  fromIntegral b4


packWord8 :: String -> Word8
packWord8 str =
  case readMaybe str :: Maybe Word8 of
    (Just n)  -> n
    Nothing   -> 0

encodeWord16 :: Word16 -> [Word8]
encodeWord16 = L.unpack . toLazyByteString . word16BE

encodeWord32 :: Word32 -> [Word8]
encodeWord32 = L.unpack . toLazyByteString . word32BE

utf8ToBytes :: String -> [Word8]
utf8ToBytes = BS.unpack . encodeUtf8 . T.pack

indexMaybe :: BS.ByteString -> Int -> Maybe Word8
indexMaybe bs n
    | n < 0                           = Nothing
    | n >= BS.length bs               = Nothing
    | otherwise                       = Just (BS.index bs n)
