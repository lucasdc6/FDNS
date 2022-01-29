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
  let lookupConfig = FC.lookup config
  addrinfos <- getAddrInfo Nothing (Just host) (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  bind sock (addrAddress serveraddr)
  logger <& ("UDP server is waiting at " ++ host ++ ":" ++ port)
  forever $ do
    (rawMessage, sockAddr) <- recvFrom sock 4096
    --logger <& ("Header: " ++ show (BS.unpack (BS.take 12 rawMessage)))
    --logger <& ("Body: " ++ show (BS.take 12 rawMessage))
    --logger <& ("Raw message: " ++ show (BS.unpack (BS.drop 12 rawMessage)))
    --logger <& ("Raw body: " ++ show (BS.drop 12 rawMessage))
    let message = unpackMessage rawMessage
    logger <& ("Message: " ++ show message)
    let question' = head (question message)
    let records = lookupConfig (qname question') (show (qtype question'))
    let recordToResource' = recordToResource (qname question') (qtype question')
    logger <& ("Records: " ++ show records)
    let answers = map recordToResource' records
    logger <& ("Answers: " ++ show answers)
    let message' = message <<! answers
    logger <& ("Message': " ++ show message')
    let response = packMessage message'
    logger <& show (BS.unpack (packMessage message))
    --logger <& ("Response: " ++ show response)
    logger <& ("Response body: " ++ show (BS.unpack (BS.drop 12 response)))
    --logger <& show (BS.drop 12 response)
    let testMessage = unpackMessage response
    logger <& ("Response parsed: " ++ show testMessage)
    sendAllTo sock response sockAddr

