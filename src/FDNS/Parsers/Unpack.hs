module FDNS.Parsers.Unpack where

import Data.Word                            (Word16)
import Data.Char                            (chr)
import Data.List                            (intersperse)
import Data.Either                          (Either, Either(Right, Left), rights, lefts)
import Data.Maybe                           (fromMaybe, fromJust)
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
  header          = header',
  question        = questions',
  answer          = answers',
  authority       = authorities',
  additional      = additionals'
}
  where (headerBytes, bodyBytes) = U.splitAt 12 rawMessage
        header                   = unpackHeader headerBytes
        questions                = unpackQuestions (qdcount header) bodyBytes
        answers                  = unpackResources (ancount header) (BS.drop questionsOffset bodyBytes)
        authorities              = unpackResources (nscount header) (BS.drop answersOffset bodyBytes)
        additionals              = unpackResources (arcount header) (BS.drop answersOffset bodyBytes)
        -- Modified DNS fields
        header'                  = setRCode header rcode'
        questions'               = rights questions
        answers'                 = rights answers
        authorities'             = rights authorities
        additionals'             = rights additionals
        -- Offsets from every field
        questionsOffset          = getOffset questionSize 0 questions'
        answersOffset            = getOffset resourceSize questionsOffset answers'
        authorityOffset          = getOffset resourceSize (questionsOffset + answersOffset) authorities'
        -- Error handling
        errors                   = lefts questions ++
                                   lefts answers ++
                                   lefts authorities ++
                                   lefts additionals
        (DNSError rcode' _)      = if length errors > 0
                                   then maximum errors
                                   else DNSError (rcode header) ""

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

unpackList :: (BS.ByteString -> (Either DNSError a, BS.ByteString))
              -> Word16
              -> BS.ByteString
              -> [Either DNSError a]
unpackList unpack 0 bytes = []
unpackList unpack n bytes = let (resource, bytes')  = unpack bytes in
                            resource : unpackList unpack (n-1) bytes'

unpackQuestions :: Word16 -> BS.ByteString -> [Either DNSError DNSQuestion]
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
unpackQuestion :: BS.ByteString -> (Either DNSError DNSQuestion, BS.ByteString)
unpackQuestion bytes =  if Nothing `elem` maybeBytes
                        then (Left $ DNSError FORMAT_ERROR "Error unpacking questions", bytes)
                        else (Right (
                            DNSQuestion{
                              qname = getName domainBytes,
                              qtype = getQType (fromJust firstByte, fromJust secondByte),
                              qclass = getQClass (fromJust thirdByte, fromJust fouthByte)
                            }), BS.drop 5 rest)
  where (domainBytes, rest) = U.span (/= '\NUL') bytes
        firstByte   = indexMaybe rest 1
        secondByte  = indexMaybe rest 2
        thirdByte   = indexMaybe rest 3
        fouthByte   = indexMaybe rest 4
        maybeBytes  = [firstByte, secondByte, thirdByte, fouthByte]

unpackResources :: Word16 -> BS.ByteString -> [Either DNSError DNSResource]
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
unpackResource :: BS.ByteString -> (Either DNSError DNSResource, BS.ByteString)
unpackResource bytes =  if Nothing `elem` maybeBytes
                        then (Left $ DNSError FORMAT_ERROR "Error unpacking resources", bytes)
                        else (Right (DNSResource{
                              rname     = getName domainBytes,
                              rtype     = rtype,
                              rclass    = getQClass (fromJust thirdByte, fromJust fouthByte),
                              ttl       = combineWords4 (fromJust fifthByte, fromJust sixthByte, fromJust seventhByte, fromJust eighthByte),
                              rdlength  = rdlength,
                              rdata     = rdata
                            }), BS.drop (fromIntegral rdlength + 11) rest)
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
        maybeBytes  = [
                        firstByte, secondByte,
                        thirdByte, fouthByte,
                        fifthByte, sixthByte,
                        seventhByte, eighthByte,
                        ninethByte, tenthByte
                      ]

