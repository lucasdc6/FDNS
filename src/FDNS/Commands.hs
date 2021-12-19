module FDNS.Commands where

import System.Environment
import System.Exit
import System.Console.GetOpt

commandHeader :: String -> String
commandHeader name = "Usage: " ++ name ++ " -c FILE [-p PORT]"

help :: IO ()
help = do
  name <- getProgName
  putStrLn (usageInfo (commandHeader name) options)

data Options = Options
  { optConfig :: FilePath
  , optPort   :: String
  , optHelp   :: Bool
  } deriving Show

defaultOptions = Options
  { optConfig = "/etc/fdns/fdns.yaml"
  , optPort   = "53"
  , optHelp   = False
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
