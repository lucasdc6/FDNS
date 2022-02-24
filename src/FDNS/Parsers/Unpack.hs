module FDNS.Parsers.Unpack where

import Data.Word                            (Word16)
import Data.Char                            (chr)
import Data.List                            (intersperse)
import Data.Maybe                           (fromMaybe)
import qualified Data.ByteString as BS      (ByteString, drop, take, unpack)
import qualified Data.ByteString.UTF8 as U  (splitAt, span)

import FDNS.Types
import FDNS.Utils
import FDNS.Parsers.Internal.Utils
import FDNS.Parsers.Internal.Unpack


{-|
-- Header format
--
--    +---------------------+
--    |        Header       |
--    +---------------------+
--    |       Question      | the question for the name server
--    +---------------------+
--    |        Answer       | RRs answering the question
--    +---------------------+
--    |      Authority      | RRs pointing toward an authority
--    +---------------------+
--    |      Additional     | RRs holding additional information
--    +---------------------+
|-}
unpackMessage :: BS.ByteString -> DNSMessage
unpackMessage rawMessage = DNSMessage{
  header          = header,
  question        = questions,
  answer          = answers,
  authority       = authorities,
  additional      = additionals
}
  where (headerBytes, bodyBytes) = U.splitAt 12 rawMessage
        header            = unpackHeader headerBytes
        questions         = unpackQuestions (qdcount header) bodyBytes
        quesrtionsOffset  = foldl (\acc q -> acc + questionSize q) 0 questions
        answers           = unpackResources (ancount header) (BS.drop quesrtionsOffset bodyBytes)
        answersOffset     = quesrtionsOffset +
                            foldl (\acc q -> acc + resourceSize q) 0 answers
        authorities       = unpackResources (nscount header) (BS.drop answersOffset bodyBytes)
        authorityOffset   = quesrtionsOffset +
                            answersOffset + foldl (\acc q -> acc + resourceSize q) 0 authorities
        additionals       = unpackResources (arcount header) (BS.drop answersOffset bodyBytes)

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
unpackHeader :: BS.ByteString -> DNSHeader
unpackHeader bytes = DNSHeader{
    identifier          = combineWords2
                          (fromMaybe 0 firstByte,
                           fromMaybe 0 secondByte),
    qr                  = getQR $ fromMaybe 0 thirdByte,
    opcode              = getOpCode $ fromMaybe 0 thirdByte,
    authoritativeAnswer = getAA (fromMaybe 0 thirdByte),
    truncatedMessage    = getTC (fromMaybe 0 thirdByte),
    recursionDesired    = getRD (fromMaybe 0 thirdByte),
    recursionAvailable  = False,
    z                   = False,
    rcode               = if Nothing `elem` maybeBytes
                          then getRCode 1
                          else getRCode $ fromMaybe 1 fouthByte,
    qdcount             = combineWords2
                          (fromMaybe 0 fifthByte,
                           fromMaybe 0 sixthByte),
    ancount             = combineWords2
                          (fromMaybe 0 seventhByte,
                           fromMaybe 0 eighthByte),
    nscount             = combineWords2
                          (fromMaybe 0 ninethByte,
                           fromMaybe 0 tenthByte),
    arcount             = combineWords2
                          (fromMaybe 0 eleventhByte,
                           fromMaybe 0 twelfthByte)
}
  where firstByte       = indexMaybe bytes 0
        secondByte      = indexMaybe bytes 1
        thirdByte       = indexMaybe bytes 2
        fouthByte       = indexMaybe bytes 3
        fifthByte       = indexMaybe bytes 4
        sixthByte       = indexMaybe bytes 5
        seventhByte     = indexMaybe bytes 6
        eighthByte      = indexMaybe bytes 7
        ninethByte      = indexMaybe bytes 8
        tenthByte       = indexMaybe bytes 9
        eleventhByte    = indexMaybe bytes 10
        twelfthByte     = indexMaybe bytes 11
        maybeBytes      = [
                            firstByte, secondByte,
                            thirdByte, fouthByte,
                            fifthByte, sixthByte,
                            seventhByte, eighthByte,
                            ninethByte, tenthByte,
                            eleventhByte, twelfthByte
                          ]

unpackList :: (BS.ByteString -> (Maybe a, BS.ByteString))
              -> Word16
              -> BS.ByteString
              -> [a]
unpackList unpack 0 bytes = []
unpackList unpack n bytes = case unpack bytes of
                          (Just resource, bytes')  ->
                            resource : unpackList unpack (n-1) bytes'
                          (Nothing, bytes')        ->
                            unpackList unpack (n-1) bytes'
  where count     = fromIntegral n

unpackQuestions = unpackList unpackQuestion

{-|
-- Questions format
--
--                                   1  1  1  1  1  1
--     0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
--   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--   |                                               |
--   /                     QNAME                     /
--   /                                               /
--   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--   |                     QTYPE                     |
--   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--   |                     QCLASS                    |
--   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|-}
unpackQuestion :: BS.ByteString -> (Maybe DNSQuestion, BS.ByteString)
unpackQuestion bytes =  if Nothing `elem` maybeBytes
                        then (Nothing, bytes)
                        else (Just (
                            DNSQuestion{
                              qname = getName domainBytes,
                              qtype = getQType (fromMaybe 0 firstByte, fromMaybe 0 secondByte),
                              qclass = getQClass (fromMaybe 0 thirdByte, fromMaybe 0 fouthByte)
                            }), BS.drop 5 rest)
  where (domainBytes, rest) = U.span (/= '\NUL') bytes
        firstByte   = indexMaybe rest 1
        secondByte  = indexMaybe rest 2
        thirdByte   = indexMaybe rest 3
        fouthByte   = indexMaybe rest 4
        maybeBytes  = [firstByte, secondByte, thirdByte, fouthByte]

unpackResources = unpackList unpackResource

{-|
-- Resources format
--
--                                   1  1  1  1  1  1
--     0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
--   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--   |                                               |
--   /                                               /
--   /                      NAME                     /
--   |                                               |
--   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--   |                      TYPE                     |
--   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--   |                     CLASS                     |
--   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--   |                      TTL                      |
--   |                                               |
--   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--   |                   RDLENGTH                    |
--   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--|
--   /                     RDATA                     /
--   /                                               /
--   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|-}
unpackResource :: BS.ByteString -> (Maybe DNSResource, BS.ByteString)
unpackResource bytes =  if Nothing `elem` maybeBytes
                        then (Nothing, bytes)
                        else (Just (DNSResource{
                              rname     = getName domainBytes,
                              rtype     = rtype,
                              rclass    = getQClass (fromMaybe 0 thirdByte, fromMaybe 0 fouthByte),
                              ttl       = combineWords4 (fromMaybe 0 fifthByte, fromMaybe 0 sixthByte, fromMaybe 0 seventhByte, fromMaybe 0 eighthByte),
                              rdlength  = rdlength,
                              rdata     = rdata
                            }), BS.drop (fromIntegral rdlength) rdataBytes)
  where (domainBytes, rest) = U.span (/= '\NUL') bytes
        firstByte   = indexMaybe rest 1
        secondByte  = indexMaybe rest 2
        thirdByte   = indexMaybe rest 3
        fouthByte   = indexMaybe rest 4
        fifthByte   = indexMaybe rest 5
        sixthByte   = indexMaybe rest 6
        seventhByte = indexMaybe rest 7
        eighthByte  = indexMaybe rest 8
        ninethByte  = indexMaybe rest 9
        tenthByte   = indexMaybe rest 10
        rtype       = getQType (fromMaybe 0 firstByte, fromMaybe 0 secondByte)
        rdlength    = combineWords2 (fromMaybe 0 ninethByte, fromMaybe 0 tenthByte)
        rdataBytes  = BS.take (fromIntegral rdlength) (BS.drop 11 rest)
        rdata       = unpackRdata rtype rdataBytes
        maybeBytes  = [firstByte, secondByte, thirdByte, fouthByte]

