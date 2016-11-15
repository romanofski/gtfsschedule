{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
