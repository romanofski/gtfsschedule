{-# LANGUAGE DeriveGeneric #-}
-- | the GTFS schedule
module Schedule
    (printSchedule
    , parseCSV
    , filterRecords
    , StopTime(..)
    , nowAsTimeOfDay
    , isIrrelevantRecord
    , isInvalidStop
    , isInvalidWeekday
    , isInvalidDepartureTime
    ) where

import Data.Csv (FromNamedRecord(..)
                , FromField(..))
import Data.Csv.Streaming
import Data.Time.LocalTime (TimeOfDay, TimeZone, utcToLocalTimeOfDay, timeToTimeOfDay)
import Data.Time.Format (parseTimeM, defaultTimeLocale, formatTime)
import Data.Time (getCurrentTime, getCurrentTimeZone)
import Data.Time.Clock (UTCTime(..))
import Control.Applicative (empty)
import GHC.Generics
import Data.List (sort, isInfixOf)
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

instance Ord StopTime where
  compare x y = compare (departure_time x) (departure_time y)

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
  (StopTime -> Bool)
  -> Records StopTime
  -> [StopTime]
filterRecords p = F.foldr (\x a -> if p x then x : a else a) []

nowAsTimeOfDay ::
  UTCTime
  -> TimeZone
  -> TimeOfDay
nowAsTimeOfDay t tz = snd $ utcToLocalTimeOfDay tz (toTimeOfDay t)
    where toTimeOfDay (UTCTime _ utcDayTime) = timeToTimeOfDay utcDayTime

-- | predicate to filter out unneeded records
--
isIrrelevantRecord ::
  String
  -> String
  -> UTCTime
  -> TimeZone
  -> StopTime
  -> Bool
isIrrelevantRecord stopID weekday now tz x = isInvalidStop stopID x
                                  && isInvalidDepartureTime now tz x
                                  && isInvalidWeekday weekday x

isInvalidStop ::
  String
  -> StopTime
  -> Bool
isInvalidStop stopID x = stop_id x == stopID

isInvalidWeekday ::
  String
  -> StopTime
  -> Bool
isInvalidWeekday weekday x = weekday `isInfixOf` trip_id x

isInvalidDepartureTime ::
  UTCTime
  -> TimeZone
  -> StopTime
  -> Bool
isInvalidDepartureTime now tz x = departure_time x >= nowAsTimeOfDay now tz

-- | shows meaningful information for leaving trains
--
printStopTimesAsSchedule ::
  [StopTime]
  -> String
printStopTimesAsSchedule (StopTime { departure_time = depTime, trip_id = tripId } : xs) =
  tripId ++ " " ++ show depTime ++ " " ++ printStopTimesAsSchedule xs
printStopTimesAsSchedule [] = []

-- | prints list of StopTimes as schedule
--
printSchedule ::
  String
  -> B.ByteString
  -> IO ()
printSchedule sId c = do
  parsed <- parseCSV c
  case parsed of
    Left err -> print err
    Right r -> do
      t <- getCurrentTime
      tz <- getCurrentTimeZone
      let weekday = formatTime defaultTimeLocale "%A" t
      let xs = sort $ filterRecords (isIrrelevantRecord sId weekday t tz) r
      print $ printStopTimesAsSchedule $ take 10 xs
