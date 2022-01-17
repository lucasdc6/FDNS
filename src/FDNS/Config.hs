module FDNS.Config where

import Data.List  (find)
import FDNS.Types

data Domain = Domain {
  name          :: String,
  recordType    :: QTYPE,
  value         :: String
} deriving (Eq, Show)

data Config = Config {
  domains :: [Domain]
} deriving (Eq, Show)

config = Config {
  domains = [
    Domain {
      name        = ".google.com",
      recordType  = A,
      value       = "172.17.0.1"
    }
  ]
}


lookup :: String -> QTYPE -> Maybe Domain
lookup name' recordType' = find (\d -> and [name d == name', recordType d == recordType']) (domains config)
