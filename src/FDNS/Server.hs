module FDNS.Server where

import Data.Word                  (Word16, Word32)
import Control.Monad              (forever)
import Network.Socket             (getAddrInfo, socket, addrAddress, addrFamily, bind, defaultProtocol, SocketType(Datagram))
import Data.ByteString as BS      (unpack, take, drop)
import Network.Socket.ByteString  (recvFrom, sendAllTo)
import Colog                      ((<&), logStringStdout)

import FDNS.Types
import FDNS.Utils
import FDNS.Commands
import FDNS.Config as FC
import FDNS.Parsers.Pack
import FDNS.Parsers.Unpack

runUDPServer :: Options -> IO ()
runUDPServer options = do
  let logger = logStringStdout
  let host = optBindAddress options
  let port = optPort options
  let configFile = optConfig options
  logger <& ("Load config file from \"" ++ configFile ++ "\"")
  config <- FC.readConfig configFile
  let resolver = dnsResolver config
  addrinfos <- getAddrInfo Nothing (Just host) (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  bind sock (addrAddress serveraddr)
  logger <& ("UDP server is waiting at " ++ host ++ ":" ++ port)
  forever $ do
    (rawMessage, sockAddr) <- recvFrom sock 4096
    let message = unpackMessage rawMessage
    logger <& ("Message: " ++ show message)
    let message' = resolver message
    let response = packMessage message'
    logger <& ("Response body: " ++ (show $ BS.unpack response))
    sendAllTo sock response sockAddr

