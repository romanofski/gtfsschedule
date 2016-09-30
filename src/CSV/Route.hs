{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module CSV.Route where

import CSV.Util (maybeToPersist)

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
