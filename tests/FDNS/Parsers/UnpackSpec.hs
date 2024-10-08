module FDNS.Parsers.UnpackSpec (spec) where

import Test.Hspec                           (Spec, shouldBe, describe, it)
import qualified Data.ByteString as BS      (pack)
import FDNS.Types
import FDNS.Parsers.Unpack

spec :: Spec
spec = do
  describe "Incomplete DNS Message header unpack" unpackIncompleteHeaderSpec
  describe "DNS Message unpack questions" unpackMessageQuestionsSpec
  describe "DNS Message unpack questions with answers" unpackMessageQuestionsAndAnwsersSpec


unpackIncompleteHeaderSpec :: Spec
unpackIncompleteHeaderSpec = do
  it "has empty" $ do
    let bytestring = BS.pack []
    let dnsMessage = DNSMessage {
      header = DNSHeader {
        identifier = 0,
        qr = False,
        opcode = QUERY,
        authoritativeAnswer = False,
        truncatedMessage = False,
        recursionDesired = False,
        recursionAvailable = False,
        z = False,
        rcode = FORMAT_ERROR,
        qdcount = 0,
        ancount = 0,
        nscount = 0,
        arcount = 0
      },
      question = [],
      answer = [],
      authority = [],
      additional = []
    }
    unpackMessage bytestring `shouldBe` dnsMessage

  it "has only the identifier" $ do
    let bytestring = BS.pack [0,1]
    let dnsMessage = DNSMessage {
      header = DNSHeader {
        identifier = 1,
        qr = False,
        opcode = QUERY,
        authoritativeAnswer = False,
        truncatedMessage = False,
        recursionDesired = False,
        recursionAvailable = False,
        z = False,
        rcode = FORMAT_ERROR,
        qdcount = 0,
        ancount = 0,
        nscount = 0,
        arcount = 0
      },
      question = [],
      answer = [],
      authority = [],
      additional = []
    }
    unpackMessage bytestring `shouldBe` dnsMessage

unpackMessageQuestionsSpec :: Spec
unpackMessageQuestionsSpec = do
  it "has only one question with type A" $ do
    let bytestring = BS.pack [0,1,1,0,0,1,0,0,0,0,0,0,7,101,120,97,109,112,108,101,3,99,111,109,0,0,1,0,1]
    let dnsMessage = DNSMessage {
      header = DNSHeader {
        identifier = 1,
        qr = False,
        opcode = QUERY,
        authoritativeAnswer = False,
        truncatedMessage = False,
        recursionDesired = True,
        recursionAvailable = False,
        z = False,
        rcode = NO_ERROR,
        qdcount = 1,
        ancount = 0,
        nscount = 0,
        arcount = 0
      },
      question = [
        DNSQuestion {qname = ".example.com", qtype = A, qclass = IN}
      ],
      answer = [],
      authority = [],
      additional = []
    }
    unpackMessage bytestring `shouldBe` dnsMessage

  it "has two question with type A" $ do
    let bytestring = BS.pack [0,1,1,0,0,2,0,0,0,0,0,0,7,101,120,97,109,112,108,101,3,99,111,109,0,0,1,0,1,7,101,120,97,109,112,108,101,3,99,111,109,2,97,114,0,0,1,0,1]
    let dnsMessage = DNSMessage {
      header = DNSHeader {
        identifier = 1,
        qr = False,
        opcode = QUERY,
        authoritativeAnswer = False,
        truncatedMessage = False,
        recursionDesired = True,
        recursionAvailable = False,
        z = False,
        rcode = NO_ERROR,
        qdcount = 2,
        ancount = 0,
        nscount = 0,
        arcount = 0
      },
      question = [
        DNSQuestion {qname = ".example.com", qtype = A, qclass = IN},
        DNSQuestion {qname = ".example.com.ar", qtype = A, qclass = IN}
      ],
      answer = [],
      authority = [],
      additional = []
    }
    unpackMessage bytestring `shouldBe` dnsMessage

  it "has only one question with type AAAA" $ do
    let bytestring = BS.pack [0,1,1,0,0,1,0,0,0,0,0,0,7,101,120,97,109,112,108,101,3,99,111,109,0,0,28,0,1]
    let dnsMessage = DNSMessage {
      header = DNSHeader {
        identifier = 1,
        qr = False,
        opcode = QUERY,
        authoritativeAnswer = False,
        truncatedMessage = False,
        recursionDesired = True,
        recursionAvailable = False,
        z = False,
        rcode = NO_ERROR,
        qdcount = 1,
        ancount = 0,
        nscount = 0,
        arcount = 0
      },
      question = [
        DNSQuestion {qname = ".example.com", qtype = AAAA, qclass = IN}
      ],
      answer = [],
      authority = [],
      additional = []
    }
    unpackMessage bytestring `shouldBe` dnsMessage

  it "has only one question with type MX" $ do
    let bytestring = BS.pack [0,1,1,0,0,1,0,0,0,0,0,0,7,101,120,97,109,112,108,101,3,99,111,109,0,0,15,0,1]
    let dnsMessage = DNSMessage {
      header = DNSHeader {
        identifier = 1,
        qr = False,
        opcode = QUERY,
        authoritativeAnswer = False,
        truncatedMessage = False,
        recursionDesired = True,
        recursionAvailable = False,
        z = False,
        rcode = NO_ERROR,
        qdcount = 1,
        ancount = 0,
        nscount = 0,
        arcount = 0
      },
      question = [
        DNSQuestion {qname = ".example.com", qtype = MX, qclass = IN}
      ],
      answer = [],
      authority = [],
      additional = []
    }
    unpackMessage bytestring `shouldBe` dnsMessage

unpackMessageQuestionsAndAnwsersSpec :: Spec
unpackMessageQuestionsAndAnwsersSpec = do
  it "has only one question and one answer with type A" $ do
    let bytestring = BS.pack [0,1,1,0,0,1,0,1,0,0,0,0,7,101,120,97,109,112,108,101,3,99,111,109,0,0,1,0,1,7,101,120,97,109,112,108,101,3,99,111,109,0,0,1,0,1,0,0,1,44,0,4,171,1,2,3]
    let dnsMessage = DNSMessage {
      header = DNSHeader {
        identifier = 1,
        qr = False,
        opcode = QUERY,
        authoritativeAnswer = False,
        truncatedMessage = False,
        recursionDesired = True,
        recursionAvailable = False,
        z = False,
        rcode = NO_ERROR,
        qdcount = 1,
        ancount = 1,
        nscount = 0,
        arcount = 0
      },
      question = [
        DNSQuestion {qname = ".example.com", qtype = A, qclass = IN}
      ],
      answer = [
        DNSResource {rname = ".example.com", rtype = A, rclass = IN, ttl = 300, rdlength = 4, rdata = "171.1.2.3"}
      ],
      authority = [],
      additional = []
    }
    unpackMessage bytestring `shouldBe` dnsMessage

  it "has two questions and one answer with type A" $ do
    let bytestring = BS.pack [0,1,1,0,0,2,0,1,0,0,0,0,7,101,120,97,109,112,108,101,3,99,111,109,0,0,1,0,1,7,101,120,97,109,112,108,101,3,99,111,109,2,97,114,0,0,1,0,1,7,101,120,97,109,112,108,101,3,99,111,109,0,0,1,0,1,0,0,1,44,0,4,171,1,2,3]
    let dnsMessage = DNSMessage {
      header = DNSHeader {
        identifier = 1,
        qr = False,
        opcode = QUERY,
        authoritativeAnswer = False,
        truncatedMessage = False,
        recursionDesired = True,
        recursionAvailable = False,
        z = False,
        rcode = NO_ERROR,
        qdcount = 2,
        ancount = 1,
        nscount = 0,
        arcount = 0
      },
      question = [
        DNSQuestion {qname = ".example.com", qtype = A, qclass = IN},
        DNSQuestion {qname = ".example.com.ar", qtype = A, qclass = IN}
      ],
      answer = [
        DNSResource {rname = ".example.com", rtype = A, rclass = IN, ttl = 300, rdlength = 4, rdata = "171.1.2.3"}
      ],
      authority = [],
      additional = []
    }
    unpackMessage bytestring `shouldBe` dnsMessage

  it "has only one question and two answer with type A" $ do
    let bytestring = BS.pack [0,1,1,0,0,1,0,2,0,0,0,0,7,101,120,97,109,112,108,101,3,99,111,109,0,0,1,0,1,7,101,120,97,109,112,108,101,3,99,111,109,0,0,1,0,1,0,0,1,44,0,4,171,1,2,3,7,101,120,97,109,112,108,101,3,99,111,109,0,0,1,0,1,0,0,1,44,0,4,171,1,2,4]
    let dnsMessage = DNSMessage {
      header = DNSHeader {
        identifier = 1,
        qr = False,
        opcode = QUERY,
        authoritativeAnswer = False,
        truncatedMessage = False,
        recursionDesired = True,
        recursionAvailable = False,
        z = False,
        rcode = NO_ERROR,
        qdcount = 1,
        ancount = 2,
        nscount = 0,
        arcount = 0
      },
      question = [
        DNSQuestion {qname = ".example.com", qtype = A, qclass = IN}
      ],
      answer = [
        DNSResource {rname = ".example.com", rtype = A, rclass = IN, ttl = 300, rdlength = 4, rdata = "171.1.2.3"},
        DNSResource {rname = ".example.com", rtype = A, rclass = IN, ttl = 300, rdlength = 4, rdata = "171.1.2.4"}
      ],
      authority = [],
      additional = []
    }
    unpackMessage bytestring `shouldBe` dnsMessage

  it "has only one question and one answer with type AAAA" $ do
    let bytestring = BS.pack [0,1,1,0,0,1,0,1,0,0,0,0,7,101,120,97,109,112,108,101,3,99,111,109,0,0,28,0,1,7,101,120,97,109,112,108,101,3,99,111,109,0,0,28,0,1,0,0,1,44,0,16,40,0,3,240,64,2,8,19,0,0,0,0,0,0,32,14]
    let dnsMessage = DNSMessage {
      header = DNSHeader {
        identifier = 1,
        qr = False,
        opcode = QUERY,
        authoritativeAnswer = False,
        truncatedMessage = False,
        recursionDesired = True,
        recursionAvailable = False,
        z = False,
        rcode = NO_ERROR,
        qdcount = 1,
        ancount = 1,
        nscount = 0,
        arcount = 0
      },
      question = [
        DNSQuestion {qname = ".example.com", qtype = AAAA, qclass = IN}
      ],
      answer = [
        DNSResource {rname = ".example.com", rtype = AAAA, rclass = IN, ttl = 300, rdlength = 16, rdata = "2800:03f0:4002:0813:0000:0000:0000:200e"}
      ],
      authority = [],
      additional = []
    }
    unpackMessage bytestring `shouldBe` dnsMessage

  it "has only one question and one answer with type MX" $ do
    let bytestring = BS.pack [0,1,1,0,0,1,0,1,0,0,0,0,7,101,120,97,109,112,108,101,3,99,111,109,0,0,15,0,1,7,101,120,97,109,112,108,101,3,99,111,109,0,0,15,0,1,0,0,1,44,0,17,0,20,2,109,120,7,101,120,97,109,112,108,101,3,99,111,109,9]
    let dnsMessage = DNSMessage {
      header = DNSHeader {
        identifier = 1,
        qr = False,
        opcode = QUERY,
        authoritativeAnswer = False,
        truncatedMessage = False,
        recursionDesired = True,
        recursionAvailable = False,
        z = False,
        rcode = NO_ERROR,
        qdcount = 1,
       ancount = 1,
        nscount = 0,
        arcount = 0
      },
      question = [
        DNSQuestion {qname = ".example.com", qtype = MX, qclass = IN}
      ],
      answer = [
        DNSResource {rname = ".example.com", rtype = MX, rclass = IN, ttl = 300, rdlength = 17, rdata = "20 .mx.example.com"}
      ],
      authority = [],
      additional = []
    }
    unpackMessage bytestring `shouldBe` dnsMessage

  it "has only one question and three answer with type MX" $ do
    let bytestring = BS.pack [0,1,1,0,0,1,0,3,0,0,0,0,7,101,120,97,109,112,108,101,3,99,111,109,0,0,15,0,1,7,101,120,97,109,112,108,101,3,99,111,109,0,0,15,0,1,0,0,1,44,0,17,0,10,2,109,120,7,101,120,97,109,112,108,101,3,99,111,109,7,101,120,97,109,112,108,101,3,99,111,109,0,0,15,0,1,0,0,1,44,0,18,0,15,3,109,120,97,7,101,120,97,109,112,108,101,3,99,111,109,7,101,120,97,109,112,108,101,3,99,111,109,0,0,15,0,1,0,0,1,44,0,18,0,20,3,109,120,98,7,101,120,97,109,112,108,101,3,99,111,109]
    let dnsMessage = DNSMessage {
      header = DNSHeader {
        identifier = 1,
        qr = False,
        opcode = QUERY,
        authoritativeAnswer = False,
        truncatedMessage = False,
        recursionDesired = True,
        recursionAvailable = False,
        z = False,
        rcode = NO_ERROR,
        qdcount = 1,
        ancount = 3,
        nscount = 0,
        arcount = 0
      },
      question = [
        DNSQuestion {qname = ".example.com", qtype = MX, qclass = IN}
      ],
      answer = [
        DNSResource {rname = ".example.com", rtype = MX, rclass = IN, ttl = 300, rdlength = 17, rdata = "10 .mx.example.com"},
        DNSResource {rname = ".example.com", rtype = MX, rclass = IN, ttl = 300, rdlength = 18, rdata = "15 .mxa.example.com"},
        DNSResource {rname = ".example.com", rtype = MX, rclass = IN, ttl = 300, rdlength = 18, rdata = "20 .mxb.example.com"}
      ],
      authority = [],
      additional = []
    }
    unpackMessage bytestring `shouldBe` dnsMessage

