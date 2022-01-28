{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module FDNS.Config where

import qualified Data.Yaml as Y
import Data.Word                  (Word16, Word32)
import Data.Yaml                  (FromJSON(..), (.:), decodeFileEither)
import Data.List                  (find)
import FDNS.Types
import FDNS.Utils


data Record = Record {
  recordType    :: String,
  value         :: String,
  recordTTL     :: Word32
} deriving (Eq, Show)

instance FromJSON Record where
  parseJSON (Y.Object v) =
    Record <$>
    v .:   "type"       <*>
    v .:   "value"      <*>
    v .:   "ttl"
  parseJSON k = fail ("Expected Object for Config value: " ++ show k)

data Domain = Domain {
  name          :: String,
  records       :: [Record]
} deriving (Eq, Show)

instance FromJSON Domain where
  parseJSON (Y.Object v) =
    Domain <$>
    v .:   "name"       <*>
    v .:   "records"
  parseJSON k = fail ("Expected Object for Config value: " ++ show k)

data Config = Config {
  domains :: [Domain]
} deriving (Eq, Show)

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$>
    v .:   "domains"
  parseJSON _ = fail "Expected Object for Config value"

readConfig :: String -> IO Config
readConfig configFilePath =
  either (error . show) id <$>
  (decodeFileEither configFilePath)

lookup :: Config -> String -> String -> [Record]
lookup config name' recordType' =
  case find (\d -> name d == name') (domains config) of
    (Just domain) -> filter (\r -> recordType r == recordType') (records domain)
    Nothing       -> []

recordToResource :: String -> QTYPE -> Record -> DNSResource
recordToResource name rtype record = DNSResource {
    rname     = name,
    rtype     = rtype,
    rclass    = IN,
    ttl       = recordTTL record,
    rdlength  = qtypeRDataLength rtype,
    rdata     = value record
}
