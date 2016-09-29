{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module CSV.Trip where

import qualified Database as DB

import Data.Csv ( FromNamedRecord
                , DefaultOrdered
                )
import GHC.Generics hiding (from)
import Data.Maybe (isJust)

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Trans.Resource (MonadResource)
import Database.Persist
import qualified Database.Persist.Sqlite as Sqlite
import qualified Data.Text as T
import Control.Monad.Trans.Reader (ReaderT)


data Trip = Trip { route_id :: !String
                 , service_id :: !String
                 , trip_id :: !String
                 , trip_headsign :: Maybe String
                 , direction_id :: Maybe Integer
                 , block_id :: Maybe String
                 , shape_id :: Maybe String
                 }
  deriving (Eq, Generic, Show)

instance FromNamedRecord Trip
instance DefaultOrdered Trip


insertIntoDB trip = do
  Just (Entity routeId _)<- Sqlite.selectFirst [DB.RouteCsvId ==. (route_id trip)] []
  insert_ $ DB.Trip { DB.tripRouteId = routeId
                   , DB.tripServiceId = (service_id trip)
                   , DB.tripCsvTripId = (trip_id trip)
                   , DB.tripHeadsign = (trip_headsign trip)
                   , DB.tripShortName = Nothing
                   , DB.tripDirectionId = (Just (isJust $ direction_id trip))
                   , DB.tripBlockId = (block_id trip)
                   , DB.tripShapeId = (shape_id trip)
                   , DB.tripWheelchairAccessible = Nothing
                   , DB.tripBikesAllowed = Nothing
}{-
insertIntoDB ::
  (MonadIO m)
  => Trip
  -> ReaderT Sqlite.SqlBackend m [Entity DB.Trip]
insertIntoDB trip =
  rawSql "insert into trip (route_id, service_id, csv_trip_id, headsign, short_name, direction_id, block_id, shape_id, wheelchair_accessible, bikes_allowed) \
       \ select route.id, ?, ?, ?, ?, ?, ?, ?, ?, ? \
       \ from route where ? = route.csv_id;"
    [ PersistText (T.pack $ service_id trip)
    , PersistText (T.pack $ trip_id trip)
    , PersistNull
    , PersistNull
    , PersistNull
    , PersistNull
    , PersistNull
    , PersistNull
    , PersistNull
    , PersistText (T.pack $ route_id trip)
    ]

-}