{-# LANGUAGE DeriveGeneric #-}

module CSV.Trip where

import qualified Database as DB

import Data.Csv ( FromNamedRecord
                , DefaultOrdered
                )
import GHC.Generics hiding (from)
import Data.Maybe (isJust)

import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Trans.Resource (MonadResource)
import Database.Esqueleto
import qualified Database.Persist.Sqlite as Sqlite


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


insertIntoDB ::
  (MonadLoggerIO m, MonadResource m)
  => Trip
  -> Sqlite.SqlPersistT m ()
insertIntoDB trip = insertSelect $ from $ \r -> do
  where_ (
    r ^. DB.RouteCsvId ==. val (route_id trip)
    )
  return $ DB.Trip <# (r ^. DB.RouteId)
    <&> val (service_id trip)
    <&> val (trip_id trip)
    <&> val (trip_headsign trip)
    <&> val Nothing
    <&> val (Just (isJust $ direction_id trip))
    <&> val (block_id trip)
    <&> val (shape_id trip)
    <&> val Nothing
    <&> val Nothing
