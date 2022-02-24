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
  value         :: String
} deriving (Eq, Show)

instance FromJSON Record where
  parseJSON (Y.Object v) =
    Record <$>
    v .:   "type"       <*>
    v .:   "value"
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

newtype Config = Config {
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
  decodeFileEither configFilePath

lookup :: Config -> DNSQuestion -> [DNSResource]
lookup config question =
  case find (\d -> name d == name') (domains config) of
    (Just domain) -> recordToResource' <$> filter (\r -> recordType r == recordType') (records domain)
    Nothing       -> []
  where name' = qname question
        recordType' = show $ qtype question
        recordToResource' = recordToResource name' (qtype question)

recordToResource :: String -> QTYPE -> Record -> DNSResource
recordToResource name rtype record = DNSResource {
    rname     = name,
    rtype     = rtype,
    rclass    = IN,
    ttl       = 300,
    rdlength  = qtypeRDataLength rtype (value record),
    rdata     = value record
}

dnsResolver :: Config -> DNSMessage -> DNSMessage
dnsResolver config message = message <<! resources
  where questions = question message
        lookupConfig = FDNS.Config.lookup config
        resources = concat $ lookupConfig <$> questions
