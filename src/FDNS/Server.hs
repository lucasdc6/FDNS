module FDNS.Server where

import Control.Monad             (forever)
import Network.Socket            (getAddrInfo, socket, addrAddress, addrFamily, bind, defaultProtocol, SocketType(Datagram))
import Network.Socket.ByteString (recvFrom, sendAllTo)
import Data.ByteString as BS     (unpack, take, drop)


import FDNS.Parsers.Parsers

runUDPServer :: String -> String -> IO ()
runUDPServer host port = do
  addrinfos <- getAddrInfo Nothing (Just host) (Just port)
  let serveraddr = Prelude.head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  bind sock (addrAddress serveraddr)
  print ("UDP server is waiting at " ++ host ++ ":" ++ port)
  forever $ do
    (rawMessage, sockAddr) <- recvFrom sock 512
    print "Header: "
    print (BS.unpack (BS.take 12 rawMessage))
    print (BS.take 12 rawMessage)
    print "Raw message: "
    print (BS.unpack (BS.drop 12 rawMessage))
    print (BS.drop 12 rawMessage)
    let message = unpackMessage rawMessage
    print "Message: "
    print message
    let response = packMessage message
    print "Response: "
    print response
    -- let testMessage = parseMessage response
    -- print "Response parsed: "
    -- print testMessage
    sendAllTo sock response sockAddr

