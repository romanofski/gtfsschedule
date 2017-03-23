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
module CSV.Import.Trip where

import CSV.Import.Util (maybeToPersist)

import Data.Csv ( FromNamedRecord
                , DefaultOrdered
                )
import GHC.Generics hiding (from)

import Data.Int (Int64)
import Database.Persist
import qualified Data.Text as T


data Trip = Trip { route_id :: !T.Text
                 , service_id :: !T.Text
                 , trip_id :: !T.Text
                 , trip_headsign :: Maybe T.Text
                 , direction_id :: Maybe Int64
                 , block_id :: Maybe T.Text
                 , shape_id :: Maybe T.Text
                 }
  deriving (Eq, Generic, Show)

instance FromNamedRecord Trip
instance DefaultOrdered Trip

prepareSQL ::
  T.Text
prepareSQL = "insert into trip (route_id, service_id, trip_id, headsign, direction_id, block_id, shape_id) \
       \ values (?, ?, ?, ?, ?, ?, ?)"

convertToValues ::
  Trip
  -> [PersistValue]
convertToValues trip = [ PersistText $ route_id trip
                       , PersistText $ service_id trip
                       , PersistText $ trip_id trip
                       , maybeToPersist PersistText (trip_headsign trip)
                       , maybeToPersist PersistInt64 (direction_id trip)
                       , maybeToPersist PersistText (block_id trip)
                       , maybeToPersist PersistText (shape_id trip)
                       ]
