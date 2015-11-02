{-# LANGUAGE DeriveGeneric #-}
-- | the GTFS schedule
module Schedule
    (getSchedule
    , printSchedule
    , isStation
    , parseCSV
    , filterRecords
    , StopTime(..)
    ) where

import Data.Csv (FromNamedRecord(..)
                , FromField(..)
                , runParser
                , (.:))
import Data.Csv.Streaming
import Data.Vector (Vector)
import Data.Time.LocalTime (TimeOfDay)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Control.Applicative (empty)
import GHC.Generics
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B
import qualified Data.Foldable as F


data StopTime = StopTime { trip_id :: !String
                         , arrival_time :: !TimeOfDay
                         , departure_time :: !TimeOfDay
                         , stop_id :: !String
                         , stop_sequence :: !Int
                         , pickup_type :: !Int
                         , drop_off_type :: !Int
                         }
              deriving (Eq, Generic, Show)

instance FromNamedRecord StopTime

instance FromField TimeOfDay where
  parseField s = case parseTimeM True defaultTimeLocale "%T" (C.unpack s) of
    Just t -> pure t
    Nothing -> empty


-- | parses CSV file and returns either a parser error or a list of stop times
--
parseCSV ::
  B.ByteString
  -> IO (Either String (Records StopTime))
parseCSV contents =
  case decodeByName contents of
    Left errmsg -> return $ Left errmsg
    Right (_, r) -> return $ Right r

filterRecords ::
  Records StopTime
  -> [StopTime]
filterRecords = F.foldr filterStation []

-- | TODO: use isStation
filterStation ::
  StopTime
  -> [StopTime]
  -> [StopTime]
filterStation x@(StopTime {stop_id = sID}) a = if sID == "600029" then x : a else a

-- | returns True if given station id is part of the stop time record
--
isStation ::
  String
  -> StopTime
  -> Bool
isStation x StopTime { stop_id = sID } = sID == x

-- | returns all available stop information
--
getSchedule ::
  String
  -> Vector StopTime
  -> Vector StopTime
getSchedule stopID = V.filter p
  where p StopTime { stop_id = sID } = sID == stopID

-- | prints list of StopTimes as schedule
--
printSchedule ::
  Vector StopTime
  -> IO (Vector ())
printSchedule = V.mapM print
