{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) - 2017 Róman Joost <roman@bromeco.de>

This file is part of gtfsschedule.

gtfsschedule is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

gtfsschedule is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with gtfsschedule.  If not, see <http://www.gnu.org/licenses/>.
-}
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
