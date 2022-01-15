module FDNS.Server where

import Data.Word                  (Word16, Word32)
import Control.Monad              (forever)
import Network.Socket             (getAddrInfo, socket, addrAddress, addrFamily, bind, defaultProtocol, SocketType(Datagram))
import Data.ByteString as BS      (unpack, take, drop)
import Network.Socket.ByteString  (recvFrom, sendAllTo)

import FDNS.Types
import FDNS.Parsers.Parsers

runUDPServer :: String -> String -> IO ()
runUDPServer host port = do
  addrinfos <- getAddrInfo Nothing (Just host) (Just port)
  let serveraddr = Prelude.head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  bind sock (addrAddress serveraddr)
  print ("UDP server is waiting at " ++ host ++ ":" ++ port)
  forever $ do
    (rawMessage, sockAddr) <- recvFrom sock 4096
    print "Header: "
    print (BS.unpack (BS.take 12 rawMessage))
    print (BS.take 12 rawMessage)
    print "Raw message: "
    print (BS.unpack (BS.drop 12 rawMessage))
    print (BS.drop 12 rawMessage)
    let message = unpackMessage rawMessage
    print "Message: "
    print message
    let answer = DNSResource {
      rname = ".google.com",
      rtype = A,
      rclass = IN,
      ttl = 300::Word32,
      rdlength = 4::Word16,
      rdata = "172.17.0.2"
    }
    let message' = appendAnswer message answer
    print "Message': "
    print message'
    let response = packMessage message'
    print "Response: "
    print response
    print "Response body:"
    print (BS.unpack (BS.drop 12 response))
    print (BS.drop 12 response)
    let testMessage = unpackMessage response
    print "Response parsed: "
    print testMessage
    sendAllTo sock response sockAddr

