module FDNS.Utils where

import Data.Bits  ((.&.), testBit)
import Data.Word  (Word8, Word16)
import FDNS.Types
import FDNS.Parsers.Internal.Utils

qtypeRDataLength :: QTYPE -> String -> Word16
qtypeRDataLength A      _ = 4
qtypeRDataLength AAAA   _ = 16
qtypeRDataLength MX rdata = fromIntegral (length rdata)
qtypeRDataLength _      _ = 0

qtypeToID :: QTYPE -> Word16
qtypeToID A           = 1
qtypeToID AAAA        = 28
qtypeToID AFSDB       = 18
qtypeToID APL         = 42
qtypeToID CAA         = 257
qtypeToID CDNSKEY     = 60
qtypeToID CDS         = 59
qtypeToID CERT        = 37
qtypeToID CNAME       = 5
qtypeToID DHCID       = 49
qtypeToID DLV         = 32769
qtypeToID DNSKEY      = 48
qtypeToID DS          = 43
qtypeToID IPSECKEY    = 45
qtypeToID KEY         = 25
qtypeToID KX          = 36
qtypeToID LOC         = 29
qtypeToID MD          = 3
qtypeToID MF          = 4
qtypeToID MB          = 7
qtypeToID MG          = 8
qtypeToID MR          = 9
qtypeToID MX          = 15
qtypeToID NAPTR       = 35
qtypeToID NS          = 2
qtypeToID NSEC        = 47
qtypeToID NSEC3       = 50
qtypeToID NSEC3PARAM  = 51
qtypeToID NULL        = 10
qtypeToID PTR         = 12
qtypeToID RRSIG       = 46
qtypeToID RP          = 17
qtypeToID SIG         = 24
qtypeToID SOA         = 6
qtypeToID SRV         = 33
qtypeToID SSHFP       = 44
qtypeToID TA          = 32768
qtypeToID TKEY        = 249
qtypeToID TLSA        = 52
qtypeToID TSIG        = 250
qtypeToID TXT         = 16
qtypeToID DNAME       = 39
qtypeToID WKS         = 11
qtypeToID HINFO       = 13
qtypeToID MINFO       = 14
qtypeToID QTYPE_ALL   = 255
qtypeToID AXFR        = 252
qtypeToID IXFR        = 251
qtypeToID OPT         = 41
qtypeToID MAILB       = 253
qtypeToID MAILA       = 254

qclassToID :: QCLASS -> Word16
qclassToID IN         = 1
qclassToID CS         = 2
qclassToID CH         = 3
qclassToID HS         = 4
qclassToID QCLASS_ALL = 255

idToQClass :: Word16 -> Maybe QCLASS
idToQClass 1          = Just IN
idToQClass 2          = Just CS
idToQClass 3          = Just CH
idToQClass 4          = Just HS
idToQClass 255        = Just QCLASS_ALL
idToQClass _          = Nothing

idToOpCode :: (Integral a) => a -> OPCODE
idToOpCode 0 = QUERY
idToOpCode 1 = IQUERY
idToOpCode 2 = STATUS
idToOpCode _ = OPCODE_OTHER

opCodeToID :: OPCODE -> Word8
opCodeToID QUERY        = 0
opCodeToID IQUERY       = 1
opCodeToID STATUS       = 2
opCodeToID OPCODE_OTHER = 3


idToRCode :: (Integral a) => a -> RCODE
idToRCode 0 = NO_ERROR
idToRCode 1 = FORMAT_ERROR
idToRCode 2 = SERVER_FAILURE
idToRCode 3 = NAME_ERROR
idToRCode 4 = NOT_IMPLEMENTED
idToRCode 5 = REFUCER
idToRCode _ = RCODE_OTHER

rCodeToId :: RCODE -> Word8
rCodeToId NO_ERROR        = 0
rCodeToId FORMAT_ERROR    = 1
rCodeToId SERVER_FAILURE  = 2
rCodeToId NAME_ERROR      = 3
rCodeToId NOT_IMPLEMENTED = 4
rCodeToId REFUCER         = 5
rCodeToId RCODE_OTHER     = 6

idToQType :: Word16 -> Maybe QTYPE
idToQType 1           = Just A
idToQType 28          = Just AAAA
idToQType 18          = Just AFSDB
idToQType 42          = Just APL
idToQType 257         = Just CAA
idToQType 60          = Just CDNSKEY
idToQType 59          = Just CDS
idToQType 37          = Just CERT
idToQType 5           = Just CNAME
idToQType 49          = Just DHCID
idToQType 32769       = Just DLV
idToQType 48          = Just DNSKEY
idToQType 43          = Just DS
idToQType 45          = Just IPSECKEY
idToQType 25          = Just KEY
idToQType 36          = Just KX
idToQType 29          = Just LOC
idToQType 15          = Just MX
idToQType 35          = Just NAPTR
idToQType 2           = Just NS
idToQType 47          = Just NSEC
idToQType 50          = Just NSEC3
idToQType 51          = Just NSEC3PARAM
idToQType 12          = Just PTR
idToQType 46          = Just RRSIG
idToQType 17          = Just RP
idToQType 24          = Just SIG
idToQType 6           = Just SOA
idToQType 33          = Just SRV
idToQType 44          = Just SSHFP
idToQType 32768       = Just TA
idToQType 249         = Just TKEY
idToQType 52          = Just TLSA
idToQType 250         = Just TSIG
idToQType 16          = Just TXT
idToQType 39          = Just DNAME
idToQType 255         = Just QTYPE_ALL
idToQType 252         = Just AXFR
idToQType 251         = Just IXFR
idToQType 41          = Just OPT
idToQType _           = Nothing

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

getQType :: (Word8, Word8) -> QTYPE
getQType words = case idToQType (combineWords2 words) of
                  (Just x)  -> x
                  Nothing   -> NULL


getQClass :: (Word8, Word8) -> QCLASS
getQClass words = case idToQClass (combineWords2 words) of
                      (Just x)  -> x
                      Nothing   -> IN

questionSize :: DNSQuestion -> Int
questionSize question = length (qname question) +
                        1 +
                        -- QTYPE size
                        2 +
                        -- QCLASS size
                        2

resourceSize :: DNSResource -> Int
resourceSize resource = length (rname resource) +
                        1 +
                        -- QTYPE size
                        2 +
                        -- QCLASS size
                        2 +
                        -- TTL size
                        4 +
                        -- RDLENGTH size
                        2 +
                        -- RDATA size
                        fromIntegral (rdlength resource)

setQueryResponse :: DNSMessage -> DNSMessage
setQueryResponse
  (DNSMessage
    (DNSHeader
      identifier
      qr
      opcode
      authoritativeAnswer
      truncatedMessage
      recursionDesired
      recursionAvailable
      z
      rccode
      qdcount
      ancount
      nscount
      arcount
    )
    question
    answer
    authority
    additional
  ) = DNSMessage {
        header = DNSHeader {
          identifier          = identifier,
          qr                  = True,
          opcode              = opcode,
          authoritativeAnswer = authoritativeAnswer,
          truncatedMessage    = truncatedMessage,
          recursionDesired    = recursionDesired,
          recursionAvailable  = recursionAvailable,
          z                   = z,
          rccode              = rccode,
          qdcount             = qdcount,
          ancount             = ancount,
          nscount             = nscount,
          arcount             = arcount
        },
        question = question,
        answer = answer,
        authority = authority,
        additional = additional
      }

setRCode :: DNSHeader -> RCODE -> DNSHeader
setRCode
  (DNSHeader
    identifier
    qr
    opcode
    authoritativeAnswer
    truncatedMessage
    recursionDesired
    recursionAvailable
    z
    _
    qdcount
    ancount
    nscount
    arcount
  ) rccode = DNSHeader {
      identifier          = identifier,
      qr                  = qr,
      opcode              = opcode,
      authoritativeAnswer = authoritativeAnswer,
      truncatedMessage    = truncatedMessage,
      recursionDesired    = recursionDesired,
      recursionAvailable  = recursionAvailable,
      z                   = z,
      rccode              = rccode,
      qdcount             = qdcount,
      ancount             = ancount,
      nscount             = nscount,
      arcount             = arcount
    }

(<<?) :: DNSMessage -> [DNSQuestion] -> DNSMessage
(<<?)
  (DNSMessage
    (DNSHeader
      identifier
      qr
      opcode
      authoritativeAnswer
      truncatedMessage
      recursionDesired
      recursionAvailable
      z
      rccode
      qdcount
      ancount
      nscount
      arcount
    )
    question
    answer
    authority
    additional
  ) questions = DNSMessage {
      header = DNSHeader {
        identifier          = identifier,
        qr                  = True,
        opcode              = opcode,
        authoritativeAnswer = authoritativeAnswer,
        truncatedMessage    = truncatedMessage,
        recursionDesired    = recursionDesired,
        recursionAvailable  = recursionAvailable,
        z                   = z,
        rccode              = rccode,
        qdcount             = qdcount + (fromIntegral (length questions)),
        ancount             = ancount,
        nscount             = nscount,
        arcount             = arcount
      },
      question = question ++ questions,
      answer = answer,
      authority = authority,
      additional = additional
    }

(<<!) :: DNSMessage -> [DNSResource] -> DNSMessage
(<<!)
  (DNSMessage
    (DNSHeader
      identifier
      qr
      opcode
      authoritativeAnswer
      truncatedMessage
      recursionDesired
      recursionAvailable
      z
      rccode
      qdcount
      ancount
      nscount
      arcount
    )
    question
    answer
    authority
    additional
  ) resources = DNSMessage {
      header = DNSHeader {
        identifier          = identifier,
        qr                  = True,
        opcode              = opcode,
        authoritativeAnswer = authoritativeAnswer,
        truncatedMessage    = truncatedMessage,
        recursionDesired    = recursionDesired,
        recursionAvailable  = recursionAvailable,
        z                   = z,
        rccode              = rccode,
        qdcount             = qdcount,
        ancount             = ancount + (fromIntegral (length resources)),
        nscount             = nscount,
        arcount             = arcount
      },
      question = question,
      answer = answer ++ resources,
      authority = authority,
      additional = additional
    }

(<<@) :: DNSMessage -> [DNSResource] -> DNSMessage
(<<@)
  (DNSMessage
    (DNSHeader
      identifier
      qr
      opcode
      authoritativeAnswer
      truncatedMessage
      recursionDesired
      recursionAvailable
      z
      rccode
      qdcount
      ancount
      nscount
      arcount
    )
    question
    answer
    authority
    additional
  ) resources = DNSMessage {
      header = DNSHeader {
        identifier          = identifier,
        qr                  = True,
        opcode              = opcode,
        authoritativeAnswer = authoritativeAnswer,
        truncatedMessage    = truncatedMessage,
        recursionDesired    = recursionDesired,
        recursionAvailable  = recursionAvailable,
        z                   = z,
        rccode              = rccode,
        qdcount             = qdcount,
        ancount             = ancount,
        nscount             = nscount + (fromIntegral (length resources)),
        arcount             = arcount
      },
      question = question,
      answer = answer,
      authority = authority ++ resources,
      additional = additional
    }

(<<+) :: DNSMessage -> [DNSResource] -> DNSMessage
(<<+)
  (DNSMessage
    (DNSHeader
      identifier
      qr
      opcode
      authoritativeAnswer
      truncatedMessage
      recursionDesired
      recursionAvailable
      z
      rccode
      qdcount
      ancount
      nscount
      arcount
    )
    question
    answer
    authority
    additional
 ) resources = DNSMessage {
      header = DNSHeader {
        identifier          = identifier,
        qr                  = True,
        opcode              = opcode,
        authoritativeAnswer = authoritativeAnswer,
        truncatedMessage    = truncatedMessage,
        recursionDesired    = recursionDesired,
        recursionAvailable  = recursionAvailable,
        z                   = z,
        rccode              = rccode,
        qdcount             = qdcount,
        ancount             = ancount,
        nscount             = nscount,
        arcount             = arcount + (fromIntegral (length (resources)))
      },
      question    = question,
      answer      = answer,
      authority   = authority,
      additional  = additional ++ resources
    }

appendQuestions       = (<<?)
appendAnswers         = (<<!)
appendAuthoritatives  = (<<@)
appendAdditionals     = (<<+)
