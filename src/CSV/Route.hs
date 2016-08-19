{-# LANGUAGE DeriveGeneric #-}
module CSV.Route where

import qualified Database as DB

import Data.Csv ( FromNamedRecord
                , DefaultOrdered
                )
import Data.Sequence (Seq, empty, singleton ,(|>))
import Data.Csv.Streaming (Records)
import GHC.Generics

import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Trans.Resource (MonadResource)
import Database.Esqueleto
import qualified Database.Persist.Sqlite as Sqlite


data Route = Route { route_id :: !String
                   , route_short_name :: !String
                   , route_long_name :: !String
                   , route_desc :: Maybe String
                   , route_type :: !String
                   , route_url :: Maybe String
                   , route_color :: Maybe String
                   , route_text_color :: Maybe String
                   }
  deriving (Eq, Generic, Show)

instance FromNamedRecord Route
instance DefaultOrdered Route


insertIntoDB ::
  (MonadLoggerIO m, MonadResource m)
  => Route
  -> ReaderT Sqlite.SqlBackend m (Key DB.Route)
insertIntoDB r = insert $ DB.Route { DB.routeCsvId = route_id r
                                   , DB.routeShortName = route_short_name r
                                   , DB.routeLongName = route_long_name r
                                   , DB.routeDesc = route_desc r
                                   , DB.routeType = route_type r
                                   , DB.routeUrl = route_url r
                                   , DB.routeColor = route_color r
                                   , DB.routeTextColor = route_text_color r
                                   }