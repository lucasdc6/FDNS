module FDNS.Parsers.Pack where

import Data.Bits                            (shiftL)
import Data.Word                            (Word8, Word16)
import Data.List.Split                      (splitOn)
import qualified Data.ByteString as BS      (ByteString, append, concat, pack, empty)
import qualified Data.ByteString.UTF8 as U  (fromString)

import FDNS.Types
import FDNS.Utils
import FDNS.Parsers.Internal.Pack
import FDNS.Parsers.Internal.Utils


packMessage :: DNSMessage -> BS.ByteString
packMessage dnsMessage  = BS.concat [headerRaw, questionsRaw, answersRaw, authorityRaw, additionalRaw]
  where headerRaw       = packHeader (header dnsMessage)
        questionsRaw    = packQuestions (question dnsMessage)
        answersRaw      = packResoureces (answer dnsMessage)
        authorityRaw    = packResoureces (authority dnsMessage)
        additionalRaw   = packResoureces (additional dnsMessage)

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
        rCodeWord               = rCodeToId (rcode header)
        secondWord              = recursionAvailableWord + zWord + rCodeWord
        qdcountWord             = encodeWord16 (qdcount header)
        ancountWord             = encodeWord16 (ancount header)
        nscountWord             = encodeWord16 (nscount header)
        arcountWord             = encodeWord16 (arcount header)


packQuestions :: [DNSQuestion] -> BS.ByteString
packQuestions = foldl (\pack question -> BS.append pack (packQuestion question)) BS.empty

packQuestion :: DNSQuestion -> BS.ByteString
packQuestion question = BS.concat [qnameBS, qtypeBS, qclassBS]
  where labels   = drop 1 (splitOn "." (qname question))
        qnameBS  = U.fromString (foldl packQName "" labels ++ "\NUL")
        qtypeBS  = BS.pack (encodeWord16 (qtypeToID (qtype question)))
        qclassBS = BS.pack (encodeWord16 (qclassToID (qclass question)))

packResoureces :: [DNSResource] -> BS.ByteString
packResoureces = foldl (\pack resource -> BS.append pack (packResourece resource)) BS.empty

packResourece :: DNSResource -> BS.ByteString
packResourece resource = BS.concat [nameBS, typeBS, classBS, ttlBS, rdlengthBS, rdataBS]
  where labels      = drop 1 (splitOn "." (rname resource))
        nameBS      = U.fromString (foldl packQName "" labels ++ "\NUL")
        typeBS      = BS.pack (encodeWord16 (qtypeToID (rtype resource)))
        classBS     = BS.pack (encodeWord16 (qclassToID (rclass resource)))
        ttlBS       = BS.pack (encodeWord32 (ttl resource))
        rdlengthBS  = BS.pack (encodeWord16 (rdlength resource))
        rdataBS     = packRData (rtype resource) (rdata resource)

