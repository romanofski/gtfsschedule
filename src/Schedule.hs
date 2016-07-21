-- | the GTFS schedule
module Schedule where

import qualified Database as DB

import Data.Time.LocalTime ( utcToLocalTime
                           , LocalTime(..)
                           , TimeOfDay(..)
                           , timeOfDayToTime
                           , timeToTimeOfDay
                           , TimeZone)
import Data.Time.Calendar (Day)
import Data.Time (getCurrentTime, getCurrentTimeZone)
import Data.Time.Clock (secondsToDiffTime, DiffTime, UTCTime)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.Esqueleto (unValue)
import qualified Database.Persist.Sqlite as Sqlite


-- | A scheduled service
data ScheduleItem = ScheduleItem { tripId :: String
                                 , stopId :: String
                                 , serviceName :: String
                                 , scheduledDepartureTime :: TimeOfDay
                                 , departureDelay :: Integer
                                 , departureTime :: TimeOfDay
                                 } deriving (Show, Eq)


getSchedule ::
  String
  -> String
  -> Integer
  -> IO [ScheduleItem]
getSchedule sqliteDBFilepath sCode delay = nextDepartures (T.pack sqliteDBFilepath) sCode =<< timespec
  where
    timespec = do
      t <- getCurrentTime
      tz <- getCurrentTimeZone
      let lday = localDay $ utcToLocalTime tz t
      let earliestTime = calculateEarliestDepartureTime t tz (delayAsDiffTime delay)
      return (earliestTime, lday)

makeSchedule ::
  [(Sqlite.Entity DB.StopTime, Sqlite.Entity DB.Trip, Sqlite.Entity DB.Route)]
  -> [ScheduleItem]
makeSchedule stops = (\(x, y, z) -> makeItem (Sqlite.entityVal x, Sqlite.entityVal y, Sqlite.entityVal z)) <$> stops
  where
    makeItem (st, t, r) = ScheduleItem { tripId = DB.tripTripId t
                                       , stopId = DB.stopTimeStop st
                                       , serviceName = DB.routeShortName r ++ " " ++ fromMaybe (DB.routeLongName r) (DB.tripHeadsign t)
                                       , scheduledDepartureTime = DB.stopTimeDepartureTime st
                                       , departureDelay = 0
                                       , departureTime = DB.stopTimeDepartureTime st
                                       }

nextDepartures ::
  T.Text
  -> String
  -> (TimeOfDay, Day)
  -> IO [ScheduleItem]
nextDepartures connstr sCode (earliest, day) = DB.runDBWithoutLogging connstr $ do
  sID <- DB.getStopID sCode
  stops <- DB.getNextDepartures (firstStopID sID) earliest day
  return $ makeSchedule stops
    where
      firstStopID xs = fromMaybe sCode (safeHead $ unValue <$> xs)

safeHead ::
  [a]
  -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

calculateEarliestDepartureTime ::
  UTCTime
  -> TimeZone
  -> DiffTime
  -> TimeOfDay
calculateEarliestDepartureTime t tz delay = timeToTimeOfDay $ timeOfDayToTime lTimeOfDay + delay
   where
     lTimeOfDay = localTimeOfDay $ utcToLocalTime tz t

delayAsDiffTime ::
  Integer
  -> DiffTime
delayAsDiffTime delay = secondsToDiffTime (delay * 60)

-- | prints list the Schedule
--
printSchedule ::
  Integer
  -> [ScheduleItem]
  -> IO ()
printSchedule walkDelay xs = do
  t <- getCurrentTime
  tz <- getCurrentTimeZone
  let lTimeOfDay = localTimeOfDay $ utcToLocalTime tz t
  putStr $ concat $ formatScheduleItem lTimeOfDay walkDelay <$> xs

formatScheduleItem ::
  TimeOfDay
  -> Integer
  -> ScheduleItem
  -> String
formatScheduleItem nowLT walkDelay item =
  delayIndicator ++ serviceName item ++ " " ++ show (minutesToDeparture item nowLT - walkDelay) ++ "min (" ++ show (departureTime item) ++ schedDepTime ++ ") "
    where
      delayIndicator = if departureDelay item > 0 then "!" else ""
      schedDepTime = if departureDelay item > 0
                     then " (" ++ show (departureDelay item) ++ "s)"
                     else ""

minutesToDeparture ::
  ScheduleItem
  -> TimeOfDay
  -> Integer
minutesToDeparture x now = round $ toRational (depTimeInSeconds - nowInSeconds) / 60
  where
    nowInSeconds = timeOfDayToTime now
    depTimeInSeconds = timeOfDayToTime $ departureTime x

secondsToDeparture ::
  TimeOfDay
  -> DiffTime
  -> DiffTime
secondsToDeparture now delay = timeOfDayToTime now + delay
