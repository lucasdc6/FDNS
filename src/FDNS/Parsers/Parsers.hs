module FDNS.Parsers.Parsers where

import Data.Bits                            (shiftL)
import Data.Word                            (Word8, Word16)
import Data.ByteString                      (ByteString, append, concat, indexMaybe, pack, empty)
import qualified Data.ByteString.Char8 as C (tail)
import qualified Data.ByteString.UTF8 as U  (splitAt, span, fromString)
import FDNS.Types
import FDNS.Parsers.Internal

unpackMessage :: ByteString -> DNSMessage
unpackMessage rawMessage = DNSMessage{
  header = header,
  question = questions,
  answer = [],
  authority = [],
  additional = []
}
  where (headerBytes, bodyBytes) = U.splitAt 12 rawMessage
        header = unpackHeader headerBytes
        questions = unpackQuestions (qdcount header) bodyBytes

packMessage :: DNSMessage -> ByteString
packMessage dnsMessage = Data.ByteString.concat [headerRaw, questionsRaw]
  where headerRaw = packHeader (header dnsMessage)
        questionsRaw = packQuestions (question dnsMessage)
        resourcesRaw = packResoureces (answer dnsMessage)


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
unpackHeader :: ByteString -> DNSHeader
unpackHeader bytes = DNSHeader{
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
  where firstByte = case indexMaybe bytes 0 of
                      (Just x)  -> x
                      Nothing   -> 0
        secondByte = case indexMaybe bytes 1 of
                      (Just x)  -> x
                      Nothing   -> 0
        thirdByte = case indexMaybe bytes 2 of
                      (Just x)  -> x
                      Nothing   -> 0
        fouthByte = case indexMaybe bytes 3 of
                      (Just x)  -> x
                      Nothing   -> 0
        fifthByte = case indexMaybe bytes 4 of
                      (Just x)  -> x
                      Nothing   -> 0
        sixthByte = case indexMaybe bytes 5 of
                      (Just x)  -> x
                      Nothing   -> 0
        seventhByte = case indexMaybe bytes 6 of
                      (Just x)  -> x
                      Nothing   -> 0
        eighthByte = case indexMaybe bytes 7 of
                      (Just x)  -> x
                      Nothing   -> 0
        ninethByte = case indexMaybe bytes 8 of
                      (Just x)  -> x
                      Nothing   -> 0
        tenthByte = case indexMaybe bytes 9 of
                      (Just x)  -> x
                      Nothing   -> 0
        eleventhByte = case indexMaybe bytes 10 of
                      (Just x)  -> x
                      Nothing   -> 0
        twelfthByte = case indexMaybe bytes 11 of
                      (Just x)  -> x
                      Nothing   -> 0

packHeader :: DNSHeader -> ByteString
packHeader header = pack (identifierWords ++ [firstWord, secondWord] ++ qdcountWord ++ ancountWord ++ nscountWord ++ arcountWord)
  where identifierWords = encodeWord16 (identifier header)
        qrWord = 128::Word8
        opcodeWord = opCodeToID QUERY `shiftL` 3
        authoritativeAnswerWord = 0::Word8
        truncatedMessageWord = 0::Word8
        recursionDesiredWord = if recursionDesired header then 1 else 0
        firstWord = qrWord + opcodeWord + authoritativeAnswerWord + truncatedMessageWord + recursionDesiredWord
        recursionAvailableWord = 128::Word8
        zWord = 0::Word8
        rcCodeWord = rCodeToId NO_ERROR
        secondWord = recursionAvailableWord + zWord + rcCodeWord
        qdcountWord = encodeWord16 (qdcount header)
        ancountWord = encodeWord16 (1::Word16)
        nscountWord = encodeWord16 (0::Word16)
        arcountWord = encodeWord16 (0::Word16)
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
unpackQuestions :: Word16 -> ByteString -> [DNSQuestion]
unpackQuestions 0 bytes = []
unpackQuestions n bytes = case unpackQuestion bytes of
                          (Just question) -> question : (unpackQuestions (n-1) bytes)
                          Nothing         -> []
  where count = fromIntegral n

unpackQuestion :: ByteString -> Maybe DNSQuestion
unpackQuestion bytes = Just (DNSQuestion{
  qname = getName domainBytes,
  qtype = qtype,
  qclass = qclass
})
  where (domainBytes, rest) = U.span (/= '\NUL') bytes
        firstByte = case indexMaybe (C.tail rest) 0 of
                      (Just x)  -> x
                      Nothing   -> 0
        secondByte = case indexMaybe (C.tail rest) 1 of
                      (Just x)  -> x
                      Nothing   -> 0
        thirdByte = case indexMaybe (C.tail rest) 2 of
                      (Just x)  -> x
                      Nothing   -> 0
        fouthByte = case indexMaybe (C.tail rest) 3 of
                      (Just x)  -> x
                      Nothing   -> 0
        qtype = getQType [firstByte, secondByte]
        qclass = getQClass [thirdByte, fouthByte]

packQuestions :: [DNSQuestion] -> ByteString
packQuestions questions = foldl (\pack question -> append pack (packQuestion question)) empty questions

packQuestion :: DNSQuestion -> ByteString
packQuestion question = Data.ByteString.concat [qnameBS, qtypeBS, qclassBS]
  where qnameBS  = U.fromString ("\ACK" ++ (qname question) ++ "\0")
        qtypeBS  = pack (encodeWord16 (qtypeToID (qtype question)))
        qclassBS = pack (encodeWord16 (qclassToID (qclass question)))

packResoureces :: [DNSResource] -> ByteString
packResoureces resources = foldl (\pack resource -> append pack (packResourece resource)) empty resources

packResourece :: DNSResource -> ByteString
packResourece resource = Data.ByteString.concat [nameBS, typeBS, classBS]
  where nameBS = U.fromString ((rname resource) ++ "\0")
        typeBS = pack (encodeWord16 (qtypeToID (rtype resource)))
        classBS = pack (encodeWord16 (qclassToID (rclass resource)))
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
--unpackResources :: ByteString -> (DNSQuestion, DNSResource, DNSResource, DNSResource)
--unpackResources _ = (DNSQuestion{}, DNSResource{}, DNSResource{}, DNSResource{})
-- print (C.unpack message)

