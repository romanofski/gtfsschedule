{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module CSV.Import.Stop where

import CSV.Import.Util (maybeToPersist)

import Data.Csv ( FromNamedRecord
                , DefaultOrdered
                )
import GHC.Generics

import Data.Int (Int64)
import Database.Persist (PersistValue(..))
import qualified Data.Text as T


data Stop = Stop { stop_id :: !T.Text
                 , stop_code :: Maybe T.Text
                 , stop_name :: !T.Text
                 , stop_desc :: Maybe T.Text
                 , stop_lat :: !Double
                 , stop_lon :: !Double
                 , zone_id :: Maybe T.Text
                 , stop_url :: Maybe T.Text
                 , location_type :: Maybe Int64
                 , parent_station :: Maybe T.Text
                 }
  deriving (Eq, Generic, Show)

instance FromNamedRecord Stop
instance DefaultOrdered Stop


prepareSQL ::
  T.Text
prepareSQL = "insert into stop (stop_id, code, name, desc, lat, lon, zone_id, url, location_type, parent_station)\
                    \ values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?);"

convertToValues ::
  Stop
  -> [PersistValue]
convertToValues r =
  [ PersistText $ stop_id r
  , maybeToPersist PersistText (stop_code r)
  , PersistText $ stop_name r
  , maybeToPersist PersistText (stop_desc r)  -- TODO: should actually be NULL?
  , PersistDouble $ stop_lat r
  , PersistDouble $ stop_lon r
  , maybeToPersist PersistText (zone_id r)
  , maybeToPersist PersistText (stop_url r)
  , maybeToPersist PersistInt64 (location_type r)
  , maybeToPersist PersistText (parent_station r)
  ]
