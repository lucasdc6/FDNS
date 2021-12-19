module FDNS.Types where

import Data.Word

data OPCODE =   QUERY
              | IQUERY
              | STATUS
              | OPCODE_OTHER deriving (Eq, Show, Enum)

data RCODE =   NO_ERROR
             | FORMAT_ERROR
             | SERVER_FAILURE
             | NAME_ERROR
             | NOT_IMPLEMENTED
             | REFUCER
             | RCODE_OTHER deriving (Eq, Show, Enum)

data QTYPE =  A
            | AAAA
            | AFSDB
            | APL
            | CAA
            | CDNSKEY
            | CDS
            | CERT
            | CNAME
            | DHCID
            | DLV
            | DNSKEY
            | DS
            | IPSECKEY
            | KEY
            | KX
            | LOC
            | MD
            | MF
            | MB
            | MG
            | MR
            | MX
            | NAPTR
            | NS
            | NSEC
            | NSEC3
            | NSEC3PARAM
            | NULL
            | PTR
            | RRSIG
            | RP
            | SIG
            | SOA
            | SRV
            | SSHFP
            | TA
            | TKEY
            | TLSA
            | TSIG
            | TXT
            | DNAME
            | WKS
            | HINFO
            | MINFO
            | QTYPE_ALL
            | AXFR
            | IXFR
            | OPT
            | MAILB
            | MAILA deriving (Eq, Show, Enum)

data COMPRESSION_FORMAT =   SEQUENCE
                          | POINTER
                          | SEQUENCE_POINTER deriving (Eq, Show)

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

data QCLASS =   IN
              | CS
              | CH
              | HS
              | QCLASS_ALL deriving (Eq, Show, Enum)

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

idToRCode :: (Integral a) => a -> RCODE
idToRCode 0 = NO_ERROR
idToRCode 1 = FORMAT_ERROR
idToRCode 2 = SERVER_FAILURE
idToRCode 3 = NAME_ERROR
idToRCode 4 = NOT_IMPLEMENTED
idToRCode 5 = REFUCER
idToRCode _ = RCODE_OTHER

data DNSHeader = DNSHeader {
  identifier          :: Word16,
  qr                  :: Bool,
  opcode              :: OPCODE,
  authoritativeAnswer :: Bool,
  truncatedMessage    :: Bool,
  recursionDesired    :: Bool,
  recursionAvailable  :: Bool,
  z                   :: Bool,
  rccode              :: RCODE,
  qdcount             :: Word16,
  ancount             :: Word16,
  nscount             :: Word16,
  arcount             :: Word16
} deriving (Show, Eq)

instance Ord DNSHeader where
  compare (DNSHeader id1 _ _ _ _ _ _ _ _ _ _ _ _) (DNSHeader id2 _ _ _ _ _ _ _ _ _ _ _ _) = compare id1 id2

data DNSQuestion = DNSQuestion {
  qname               :: String,
  qtype               :: QTYPE,
  qclass              :: QCLASS
} deriving (Show, Eq)

data DNSResource = DNSResource {
  rname               :: String,
  rtype               :: QTYPE,
  rclass              :: QCLASS,
  ttl                 :: Word32,
  rdlength            :: Word16,
  rdata               :: String
} deriving (Show, Eq)

data DNSMessage = DNSMessage {
  header              :: DNSHeader,
  question            :: [DNSQuestion],
  answer              :: [DNSResource],
  authority           :: [DNSResource],
  additional          :: [DNSResource]
} deriving (Show, Eq)

instance Ord DNSMessage where
  compare (DNSMessage h1 _ _ _ _) (DNSMessage h2 _ _ _ _) = compare h1 h2

