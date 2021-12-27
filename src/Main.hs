module Main where


import System.Console.GetOpt
import System.Environment
import FDNS.Commands
import FDNS.Server

main :: IO ()
main = do
  args <- getArgs
  flags <- parseArgs args
  if optHelp flags then help else 
    runUDPServer "0.0.0.0" (optPort flags)
    --putStrLn ("Starting server at " ++ optPort flags ++ " with config file " ++ optConfig flags)
