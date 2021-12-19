module FDNS.Server where

import Control.Monad             (forever)
import Network.Socket hiding     (recv)
import Network.Socket.ByteString (recv, sendAll)
import Data.ByteString.Char8 as C
import Data.ByteString as BS
import Data.ByteString.Short as B


import FDNS.Parsers.Parsers

runUDPServer :: String -> String -> IO ()
runUDPServer host port = do
  addrinfos <- getAddrInfo Nothing (Just host) (Just port)
  let serveraddr = Prelude.head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  bind sock (addrAddress serveraddr)
  print ("UDP server is waiting at " ++ host ++ ":" ++ port)
  forever $ do
    rawMessage <- recv sock 512
    print "Header: "
    print (BS.unpack (BS.take 12 rawMessage))
    print (BS.take 12 rawMessage)
    print "Raw message: "
    print (BS.unpack (BS.drop 12 rawMessage))
    print (BS.drop 12 rawMessage)
    let message = parseMessage rawMessage
    print "Message: "
    print message


