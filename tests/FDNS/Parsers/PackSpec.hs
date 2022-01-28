module FDNS.Parsers.PackSpec (spec) where

import Test.Hspec                           (Spec, shouldBe, describe, it)
import qualified Data.ByteString as BS      (pack)
import FDNS.Types
import FDNS.Parsers.Pack

spec :: Spec
spec = do
  describe "DNS Message Pack" $ do
    it "has a DNSMessage with only one question" $ do
      let bytes = [0,1,1,128,0,1,0,0,0,0,0,0,7,101,120,97,109,112,108,101,3,99,111,109,0,0,1,0,1]
      let dnsMessage = DNSMessage {
        header = DNSHeader {
          identifier = 1,
          qr = False,
          opcode = QUERY,
          authoritativeAnswer = False,
          truncatedMessage = False,
          recursionDesired = True,
          recursionAvailable = True,
          z = False,
          rccode = NO_ERROR,
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
      (packMessage dnsMessage) `shouldBe` BS.pack bytes

    it "has a DNSMessage with two question" $ do
      let bytes = [0,1,1,128,0,2,0,0,0,0,0,0,7,101,120,97,109,112,108,101,3,99,111,109,0,0,1,0,1,7,101,120,97,109,112,108,101,3,99,111,109,2,97,114,0,0,1,0,1]
      let dnsMessage = DNSMessage {
        header = DNSHeader {
          identifier = 1,
          qr = False,
          opcode = QUERY,
          authoritativeAnswer = False,
          truncatedMessage = False,
          recursionDesired = True,
          recursionAvailable = True,
          z = False,
          rccode = NO_ERROR,
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
      (packMessage dnsMessage) `shouldBe` BS.pack bytes

    it "has a DNSMessage with only one question and one answer" $ do
      let bytes = [0,1,1,128,0,1,0,1,0,0,0,0,7,101,120,97,109,112,108,101,3,99,111,109,0,0,1,0,1,7,101,120,97,109,112,108,101,3,99,111,109,0,0,1,0,1,0,0,1,44,0,4,171,1,2,3]
      let dnsMessage = DNSMessage {
        header = DNSHeader {
          identifier = 1,
          qr = False,
          opcode = QUERY,
          authoritativeAnswer = False,
          truncatedMessage = False,
          recursionDesired = True,
          recursionAvailable = True,
          z = False,
          rccode = NO_ERROR,
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
      (packMessage dnsMessage) `shouldBe` BS.pack bytes

    it "has a DNSMessage with two questions and one answer" $ do
      let bytes = [0,1,1,128,0,2,0,1,0,0,0,0,7,101,120,97,109,112,108,101,3,99,111,109,0,0,1,0,1,7,101,120,97,109,112,108,101,3,99,111,109,2,97,114,0,0,1,0,1,7,101,120,97,109,112,108,101,3,99,111,109,0,0,1,0,1,0,0,1,44,0,4,171,1,2,3]
      let dnsMessage = DNSMessage {
        header = DNSHeader {
          identifier = 1,
          qr = False,
          opcode = QUERY,
          authoritativeAnswer = False,
          truncatedMessage = False,
          recursionDesired = True,
          recursionAvailable = True,
          z = False,
          rccode = NO_ERROR,
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
      (packMessage dnsMessage) `shouldBe` BS.pack bytes

