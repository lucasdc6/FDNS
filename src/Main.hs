module Main where


import System.Environment (getArgs)
import FDNS.Commands
import FDNS.Server

main :: IO ()
main = do
  args <- getArgs
  flags <- parseArgs args
  if optHelp flags then help else
    runUDPServer flags

