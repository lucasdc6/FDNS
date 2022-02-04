module FDNS.UtilsSpec (spec) where

import Test.Hspec                           (Spec, shouldBe, describe, it)
import FDNS.Types
import FDNS.Utils

spec :: Spec
spec = do
  describe "DNS Message Utils" manipulateDNSMessagesSpec


manipulateDNSMessagesSpec :: Spec
manipulateDNSMessagesSpec = do
  it "has a DNSMessage without answers and we need to append one answer" $ do
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
    let dnsResource = DNSResource {
      rname = ".example.com",
      rtype = A,
      rclass = IN,
      ttl = 300,
      rdlength = 4,
      rdata = "171.1.2.3"
    }
    ancount (header dnsMessage) `shouldBe` 0

    let dnsMessage' = dnsMessage <<! [dnsResource]

    ancount (header dnsMessage') `shouldBe` 1
    answer dnsMessage' `shouldBe` [dnsResource]

  it "has a DNSMessage without answers and we need to append two answers" $ do
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
    let resource1 = DNSResource{rname = ".example.com", rtype = A, rclass = IN, ttl = 300, rdlength = 4, rdata = "171.1.2.3"}
    let resource2 = DNSResource{rname = ".example.com", rtype = A, rclass = IN, ttl = 300, rdlength = 4, rdata = "171.1.2.4"}
    let dnsResources = [resource1, resource2]
    ancount (header dnsMessage) `shouldBe` 0

    let dnsMessage' = dnsMessage <<! dnsResources

    ancount (header dnsMessage') `shouldBe` 2
    answer dnsMessage' `shouldBe` dnsResources
