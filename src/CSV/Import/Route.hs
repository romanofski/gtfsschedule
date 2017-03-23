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
module CSV.Import.Route where

import CSV.Import.Util (maybeToPersist)

import Data.Csv ( FromNamedRecord
                , DefaultOrdered
                )
import GHC.Generics

import Database.Persist (PersistValue(..))
import qualified Data.Text as T


data Route = Route { route_id :: !T.Text
                   , route_short_name :: !T.Text
                   , route_long_name :: !T.Text
                   , route_desc :: Maybe T.Text
                   , route_type :: !T.Text
                   , route_url :: Maybe T.Text
                   , route_color :: Maybe T.Text
                   , route_text_color :: Maybe T.Text
                   }
  deriving (Eq, Generic, Show)

instance FromNamedRecord Route
instance DefaultOrdered Route


prepareSQL ::
  T.Text
prepareSQL = "insert into route (route_id, short_name, long_name, desc, type, url, color, text_color)\
                    \ values (?, ?, ?, ?, ?, ?, ?, ?);"

convertToValues ::
  Route
  -> [PersistValue]
convertToValues r = [ PersistText $ route_id r
                    , PersistText $ route_short_name r
                    , PersistText $ route_long_name r
                    , maybeToPersist PersistText $ route_desc r
                    , PersistText $ route_type r
                    , maybeToPersist PersistText $ route_url r
                    , maybeToPersist PersistText $ route_color r
                    , maybeToPersist PersistText $ route_text_color r
                    ]
