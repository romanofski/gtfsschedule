{-# LANGUAGE DeriveGeneric #-}
module CSV.Stop where

import qualified Database as DB

import Data.Csv ( FromNamedRecord
                , DefaultOrdered
                )
import GHC.Generics

import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Trans.Resource (MonadResource)
import Database.Esqueleto hiding (desc)
import qualified Database.Persist.Sqlite as Sqlite


data Stop = Stop { stop_id :: !String
                 , code :: Maybe String
                 , name :: !String
                 , desc :: Maybe String
                 , lat :: !Double
                 , lon :: !Double
                 , zone_id :: Maybe String
                 , url :: Maybe String
                 , location_type :: Maybe Int
                 , parent_station :: Maybe String
                 }
  deriving (Eq, Generic, Show)

instance FromNamedRecord Stop
instance DefaultOrdered Stop


insertIntoDB ::
  (MonadLoggerIO m, MonadResource m)
  => Stop
  -> ReaderT Sqlite.SqlBackend m (Key DB.Stop)
insertIntoDB r = insert $ DB.Stop { DB.stopCsvStopId = stop_id r
                                  , DB.stopCode = code r
                                  , DB.stopName = name r
                                  , DB.stopDesc = desc r
                                  , DB.stopLat = lat r
                                  , DB.stopLon = lon r
                                  , DB.stopZoneId = zone_id r
                                  , DB.stopUrl = url r
                                  , DB.stopLocationType = location_type r
                                  , DB.stopParentStation = parent_station r
                                  }