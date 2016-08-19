{-# LANGUAGE DeriveGeneric #-}
module CSV.StopTime (insertIntoDB) where

import qualified Database as DB

import Data.Csv ( FromNamedRecord
                , DefaultOrdered
                , FromField(..)
                )
import Control.Applicative (empty)
import GHC.Generics hiding (from)
import Data.Time.LocalTime ( TimeOfDay )
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import qualified Data.ByteString.Char8 as C

import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Trans.Resource (MonadResource)
import Database.Esqueleto
import qualified Database.Persist.Sqlite as Sqlite

data StopTime = StopTime { trip_id :: !String
                         , arrival_time :: !TimeOfDay
                         , departure_time :: !TimeOfDay
                         , stop_id :: !String
                         , stop_sequence :: !Int
                         , pickup_type :: Maybe Int
                         , drop_off_type :: Maybe Int
                         }
  deriving (Eq, Generic, Show)

instance FromNamedRecord StopTime
instance DefaultOrdered StopTime

instance FromField TimeOfDay where
  parseField s = case parseTimeM True defaultTimeLocale "%T" (C.unpack s) of
    Just t -> pure t
    Nothing -> empty

insertIntoDB ::
  (MonadLoggerIO m, MonadResource m)
  => StopTime
  -> Sqlite.SqlPersistT m ()
insertIntoDB st = insertSelect $ from $ \(t, s) -> do
  where_ (
    t ^. DB.TripCsvTripId ==. val (trip_id st) &&.
    s ^. DB.StopCsvStopId ==. val (stop_id st)
    )
  return $ DB.StopTime <# (t ^. DB.TripId)
    <&> val (trip_id st)
    <&> val (arrival_time st)
    <&> val (departure_time st)
    <&> val (stop_id st)
    <&> (s ^. DB.StopId)
    <&> val (stop_sequence st)
    <&> val (pickup_type st)
    <&> val (drop_off_type st)