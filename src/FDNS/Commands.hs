module FDNS.Commands where

import System.Environment     (getProgName)
import System.Console.GetOpt  (usageInfo, getOpt, OptDescr(Option), ArgOrder(Permute), ArgDescr(NoArg, ReqArg))

commandHeader :: String -> String
commandHeader name = "Usage: " ++ name ++ " -c FILE [-p PORT]"

help :: IO ()
help = do
  name <- getProgName
  putStrLn (usageInfo (commandHeader name) options)

data Options = Options
  { optConfig       :: FilePath
  , optBindAddress  :: String
  , optPort         :: String
  , optHelp         :: Bool
  } deriving Show

defaultOptions = Options
  { optConfig       = "/etc/fdns/fdns.yaml"
  , optBindAddress  = "0.0.0.0"
  , optPort         = "53"
  , optHelp         = False
  }

data Flag = Help | Config String
            deriving (Eq, Show)


options :: [OptDescr (Options -> Options)]
options = [ Option
              ['h', '?'] ["help"]
              (NoArg (\opts -> opts { optHelp = True }))
              "Show this help message"
          , Option
              ['c'] ["config"]
              (ReqArg (\config opts -> opts { optConfig = config}) "FILE")
              "Config file"
          , Option
              ['H'] ["host"]
              (ReqArg (\host opts -> opts { optBindAddress = host}) "HOST")
              "Config file"
          , Option
              ['p'] ["port"]
              (ReqArg (\port opts -> opts { optPort = port}) "PORT")
              "Config file"
          ]

parseArgs :: [String] -> IO Options
parseArgs args =
  case getOpt Permute options args of
    ([], _, [])             -> return (defaultOptions {optHelp = True})
    (opts, files, [])       -> return (foldl (flip id) defaultOptions opts)
    (_, _, errs)            -> ioError (userError (concat errs ++ usageInfo "Error parsing args" options))
