module FDNS.Parsers.Parsers where

import Data.Bits                            (shiftL)
import Data.Word                            (Word8, Word16)
import Data.Char                            (chr)
import Data.List                            (intersperse)
import Data.List.Split                      (splitOn)
import Data.Maybe                           (fromMaybe)
import qualified Data.ByteString as BS      (ByteString, append, concat, drop, pack, empty, take, unpack)
import qualified Data.ByteString.Char8 as C (tail)
import qualified Data.ByteString.UTF8 as U  (splitAt, span, fromString)
import FDNS.Types
import FDNS.Parsers.Internal


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
        quesrtionsOffset  = (foldl (\acc q -> acc + questionSize q) 0 questions)
        answers           = unpackResources (ancount header) (BS.drop quesrtionsOffset bodyBytes)
        answersOffset     = quesrtionsOffset + (foldl (\acc q -> acc + resourceSize q) 0 answers)
        authorities       = unpackResources (nscount header) (BS.drop answersOffset bodyBytes)
        authorityOffset   = quesrtionsOffset + answersOffset + (foldl (\acc q -> acc + resourceSize q) 0 authorities)
        additionals       = unpackResources (arcount header) (BS.drop answersOffset bodyBytes)

packMessage :: DNSMessage -> BS.ByteString
packMessage dnsMessage  = BS.concat [headerRaw, questionsRaw, answersRaw, authorityRaw, additionalRaw]
  where headerRaw       = packHeader (header dnsMessage)
        questionsRaw    = packQuestions (question dnsMessage)
        answersRaw      = packResoureces (answer dnsMessage)
        authorityRaw    = packResoureces (authority dnsMessage)
        additionalRaw   = packResoureces (additional dnsMessage)


{-|
-- Header formatmap
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
    identifier          = combineWords [fromMaybe 0 firstByte, fromMaybe 0 secondByte],
    qr                  = getQR (fromMaybe 0 thirdByte),
    opcode              = getOpCode (fromMaybe 0 thirdByte),
    authoritativeAnswer = getAA (fromMaybe 0 thirdByte),
    truncatedMessage    = getTC (fromMaybe 0 thirdByte),
    recursionDesired    = getRD (fromMaybe 0 thirdByte),
    recursionAvailable  = True,
    z                   = False,
    rccode              = if elem Nothing maybeBytes then getRCode 1 else getRCode (fromMaybe 1 fouthByte),
    qdcount             = combineWords [fromMaybe 0  fifthByte, fromMaybe 0 sixthByte],
    ancount             = combineWords [fromMaybe 0 seventhByte, fromMaybe 0 eighthByte],
    nscount             = combineWords [fromMaybe 0 ninethByte, fromMaybe 0 tenthByte],
    arcount             = combineWords [fromMaybe 0 eleventhByte, fromMaybe 0 twelfthByte]
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
        maybeBytes      = [firstByte, secondByte, thirdByte, fouthByte, fifthByte, sixthByte, seventhByte, eighthByte, ninethByte, tenthByte, eleventhByte, twelfthByte]

packHeader :: DNSHeader -> BS.ByteString
packHeader header = BS.pack (identifierWords ++ [firstWord, secondWord] ++ qdcountWord ++ ancountWord ++ nscountWord ++ arcountWord)
  where identifierWords         = encodeWord16 (identifier header)
        qrWord                  = if qr header then 128::Word8 else 0::Word8
        opcodeWord              = opCodeToID (opcode header) `shiftL` 3
        authoritativeAnswerWord = if authoritativeAnswer header then 4::Word8 else 0::Word8
        truncatedMessageWord    = if truncatedMessage header then 2::Word8 else 0::Word8
        recursionDesiredWord    = if recursionDesired header then 1 else 0
        firstWord               = qrWord + opcodeWord + authoritativeAnswerWord + truncatedMessageWord + recursionDesiredWord
        recursionAvailableWord  = if recursionAvailable header then 128::Word8 else 0::Word8
        zWord                   = 0::Word8
        rcCodeWord              = rCodeToId (rccode header)
        secondWord              = recursionAvailableWord + zWord + rcCodeWord
        qdcountWord             = encodeWord16 (qdcount header)
        ancountWord             = encodeWord16 (1::Word16)
        nscountWord             = encodeWord16 (0::Word16)
        arcountWord             = encodeWord16 (0::Word16)

unpackQuestions :: Word16 -> BS.ByteString -> [DNSQuestion]
unpackQuestions 0 bytes = []
unpackQuestions n bytes = case unpackQuestion bytes of
                          (Just question) -> question : (unpackQuestions (n-1) bytes)
                          Nothing         -> []
  where count = fromIntegral n

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
unpackQuestion :: BS.ByteString -> Maybe DNSQuestion
unpackQuestion bytes =  if elem Nothing maybeBytes
                        then Nothing
                        else Just (DNSQuestion{
                              qname = getName domainBytes,
                              qtype = getQType [fromMaybe 0 firstByte, fromMaybe 0 secondByte],
                              qclass = getQClass [fromMaybe 0 thirdByte, fromMaybe 0 fouthByte]
                            })
  where (domainBytes, rest) = U.span (/= '\NUL') bytes
        firstByte   = indexMaybe (C.tail rest) 0
        secondByte  = indexMaybe (C.tail rest) 1
        thirdByte   = indexMaybe (C.tail rest) 2
        fouthByte   = indexMaybe (C.tail rest) 3
        maybeBytes  = [firstByte, secondByte, thirdByte, fouthByte]

packQuestions :: [DNSQuestion] -> BS.ByteString
packQuestions questions = foldl (\pack question -> BS.append pack (packQuestion question)) BS.empty questions

packQuestion :: DNSQuestion -> BS.ByteString
packQuestion question = BS.concat [qnameBS, qtypeBS, qclassBS]
  where labels   = drop 1 (splitOn "." (qname question))
        qnameBS  = U.fromString ((foldl transformQName' "" labels) ++ "\NUL")
        qtypeBS  = BS.pack (encodeWord16 (qtypeToID (qtype question)))
        qclassBS = BS.pack (encodeWord16 (qclassToID (qclass question)))

unpackResources :: Word16 -> BS.ByteString -> [DNSResource]
unpackResources 0 bytes = []
unpackResources n bytes = case unpackResource bytes of
                          (Just resource) -> resource : (unpackResources (n-1) bytes)
                          Nothing         -> []
  where count     = fromIntegral n

{-|
-- Resources formatmap (\byte -> chr (fromIntegral byte)) rdataBytes)
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
unpackResource :: BS.ByteString -> Maybe DNSResource
unpackResource bytes =  if elem Nothing maybeBytes
                        then Nothing
                        else Just (DNSResource{
                              rname = getName domainBytes,
                              rtype = getQType [fromMaybe 0 firstByte, fromMaybe 0 secondByte],
                              rclass = getQClass [fromMaybe 0 thirdByte, fromMaybe 0 fouthByte],
                              ttl = combineWords' (fromMaybe 0 fifthByte, fromMaybe 0 sixthByte, fromMaybe 0 seventhByte, fromMaybe 0 eighthByte),
                              rdlength = fromIntegral (length rdataBytes),
                              rdata = intersperse '.' rdataBytes
                            })
  where (domainBytes, rest) = U.span (/= '\NUL') bytes
        firstByte   = indexMaybe (C.tail rest) 0
        secondByte  = indexMaybe (C.tail rest) 1
        thirdByte   = indexMaybe (C.tail rest) 2
        fouthByte   = indexMaybe (C.tail rest) 3
        fifthByte   = indexMaybe (C.tail rest) 4
        sixthByte   = indexMaybe (C.tail rest) 5
        seventhByte = indexMaybe (C.tail rest) 6
        eighthByte  = indexMaybe (C.tail rest) 7
        ninethByte  = indexMaybe (C.tail rest) 8
        tenthByte   = indexMaybe (C.tail rest) 9
        rdataBytes  = map (\byte -> chr (fromIntegral byte)) (BS.unpack (BS.take 4 (BS.drop 10 rest)))
        maybeBytes  = [firstByte, secondByte, thirdByte, fouthByte]

packResoureces :: [DNSResource] -> BS.ByteString
packResoureces resources = foldl (\pack resource -> BS.append pack (packResourece resource)) BS.empty resources

packResourece :: DNSResource -> BS.ByteString
packResourece resource = BS.concat [nameBS, typeBS, classBS, ttlBS, rdlengthBS, rdataBS]
  where labels      = drop 1 (splitOn "." (rname resource))
        nameBS      = U.fromString ((foldl transformQName' "" labels) ++ "\NUL")
        typeBS      = BS.pack (encodeWord16 (qtypeToID (rtype resource)))
        classBS     = BS.pack (encodeWord16 (qclassToID (rclass resource)))
        ttlBS       = BS.pack (encodeWord32 (ttl resource))
        rdlengthBS  = BS.pack (encodeWord16 (rdlength resource))
        rdataBS     = BS.pack (map (\str -> read str :: Word8) (splitOn "." (rdata resource)))

