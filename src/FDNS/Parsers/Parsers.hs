module FDNS.Parsers.Parsers where

import Control.Monad
import Data.Word
import Data.ByteString hiding (head, concat)
import qualified Data.ByteString.Char8 as C (splitAt, foldl, span, tail)
import FDNS.Types
import FDNS.Parsers.Internal

parseMessage :: ByteString -> DNSMessage
parseMessage rawMessage = DNSMessage{
  header = header,
  question = questions,
  answer = [],
  authority = [],
  additional = []
}
  where (headerBytes, bodyBytes) = C.splitAt 12 rawMessage
        header = parseHeader headerBytes
        questions = parseQuestions (qdcount header) bodyBytes

--packMessage :: DNSMessage -> ByteString
--packMessage dnsMessage = 
--  where header = packHeader dnsMessage


{-|
-- Header format
--
--                                   1  1  1  1  1  1
--     0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
--   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--   |                      ID                       |
--   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--   |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
--   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--   |                    QDCOUNT                    |
--   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--   |                    ANCOUNT                    |
--   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--   |                    NSCOUNT                    |
--   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--   |                    ARCOUNT                    |
--   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|-}
parseHeader :: ByteString -> DNSHeader
parseHeader bytes = DNSHeader{
    identifier = combineWords [firstByte, secondByte],
    qr = getQR thirdByte,
    opcode = getOpCode thirdByte,
    authoritativeAnswer = getAA thirdByte,
    truncatedMessage = getTC thirdByte,
    recursionDesired = getRD thirdByte,
    recursionAvailable = True,
    z = False,
    rccode = getRCode fouthByte,
    qdcount= combineWords [fifthByte, sixthByte],
    ancount = combineWords [seventhByte, eighthByte],
    nscount = combineWords [ninethByte, tenthByte],
    arcount = combineWords [eleventhByte, twelfthByte]
}
  where firstByte = index bytes 0
        secondByte = index bytes 1
        thirdByte = index bytes 2
        fouthByte = index bytes 3
        fifthByte = index bytes 4
        sixthByte = index bytes 5
        seventhByte = index bytes 6
        eighthByte = index bytes 7
        ninethByte = index bytes 8
        tenthByte = index bytes 9
        eleventhByte = index bytes 10
        twelfthByte = index bytes 11

packHeader :: DNSHeader -> String
packHeader header = "TODO"

parseQuestions :: Word16 -> ByteString -> [DNSQuestion]
parseQuestions 0 bytes = []
parseQuestions n bytes = case parseQuestion bytes of
                          (Just question) -> question : (parseQuestions (n-1) bytes)
                          Nothing         -> []
  where count = fromIntegral n

packQuestions :: [DNSQuestion] -> String
packQuestions questions = Prelude.foldl (\pack question -> pack ++ packQuestion question) "" questions

parseQuestion :: ByteString -> Maybe DNSQuestion
parseQuestion bytes = Just (DNSQuestion{
  qname = getName domainBytes,
  qtype = qtype,
  qclass = qclass
})
  where (domainBytes, rest) = C.span (/= '\NUL') bytes
        firstByte = index (C.tail rest) 0
        secondByte = index (C.tail rest) 1
        thirdByte = index (C.tail rest) 2
        fouthByte = index (C.tail rest) 3
        qtype = getQType [firstByte, secondByte]
        qclass = getQClass [thirdByte, fouthByte]

packQuestion :: DNSQuestion -> String
packQuestion question = "TODO"

--parseResources :: ByteString -> (DNSQuestion, DNSResource, DNSResource, DNSResource)
--parseResources _ = (DNSQuestion{}, DNSResource{}, DNSResource{}, DNSResource{})
-- print (C.unpack message)

