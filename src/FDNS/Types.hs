module FDNS.Types where

import Data.Word          (Word8, Word16, Word32)

data OPCODE =   QUERY
              | IQUERY
              | STATUS
              | OPCODE_OTHER deriving (Eq, Show, Enum)

data RCODE =  RCODE_OTHER
              |  NO_ERROR
              | FORMAT_ERROR
              | SERVER_FAILURE
              | NAME_ERROR
              | NOT_IMPLEMENTED
              | REFUCER deriving (Eq, Show, Enum)

data QTYPE =    A
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

data QCLASS =   IN
              | CS
              | CH
              | HS
              | QCLASS_ALL deriving (Eq, Show, Enum)


data DNSHeader = DNSHeader {
  identifier          :: Word16,
  qr                  :: Bool,
  opcode              :: OPCODE,
  authoritativeAnswer :: Bool,
  truncatedMessage    :: Bool,
  recursionDesired    :: Bool,
  recursionAvailable  :: Bool,
  z                   :: Bool,
  rcode               :: RCODE,
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

data DNSError =
  DNSError RCODE String
  deriving (Eq, Show)

instance Ord DNSError where
  compare (DNSError rcode1 _) (DNSError rcode2 _) = compare (fromEnum rcode1) (fromEnum rcode2)
