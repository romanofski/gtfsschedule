{- | This module provides schedule information. The information is primarily retrieved from the static schedule (e.g. from the database), but is updated with realtime information.
-}
module Schedule where

import qualified Database as DB

import Data.Functor ((<$>))

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


-- | A poor mans data type to express the state of the service
data ScheduleType = CANCELED
                  | ADDED
                  | SCHEDULED
                  deriving (Show, Eq, Ord)

data ScheduleItem = ScheduleItem { tripId :: String
                                 , stopId :: String
                                 , serviceName :: String
                                 , scheduledDepartureTime :: TimeOfDay
                                 , departureDelay :: Integer
                                 , departureTime :: TimeOfDay
                                 , scheduleType :: ScheduleType
                                 } deriving (Show, Eq, Ord)

-- | A specific point in time from when we want to calculate the next departing
--   services
data TimeSpec = TimeSpec TimeOfDay Day


getSchedule ::
  String
  -> String
  -> TimeSpec
  -> IO [ScheduleItem]
getSchedule sqliteDBFilepath sCode timespec = nextDepartures (T.pack sqliteDBFilepath) sCode timespec

getTimeSpecFromNow ::
  Integer
  -> IO TimeSpec
getTimeSpecFromNow delay = do
      t <- getCurrentTime
      tz <- getCurrentTimeZone
      let lday = localDay $ utcToLocalTime tz t
      let earliestTime = calculateEarliestDepartureTime t tz (delayAsDiffTime delay)
      return $ TimeSpec earliestTime lday

makeSchedule ::
  [(Sqlite.Entity DB.StopTime, Sqlite.Entity DB.Trip, Sqlite.Entity DB.Route)]
  -> [ScheduleItem]
makeSchedule stops = (\(x, y, z) -> makeItem (Sqlite.entityVal x, Sqlite.entityVal y, Sqlite.entityVal z)) <$> stops
  where
    makeItem (st, t, r) = ScheduleItem { tripId = DB.tripTripId t
                                       , stopId = DB.stopTimeStopId st
                                       , serviceName = DB.routeShortName r ++ " " ++ fromMaybe (DB.routeLongName r) (DB.tripHeadsign t)
                                       , scheduledDepartureTime = DB.stopTimeDepartureTime st
                                       , departureDelay = 0
                                       , departureTime = DB.stopTimeDepartureTime st
                                       , scheduleType = SCHEDULED
                                       }

nextDepartures ::
  T.Text
  -> String
  -> TimeSpec
  -> IO [ScheduleItem]
nextDepartures connstr sCode (TimeSpec earliest day) = DB.runDBWithoutLogging connstr $ do
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
formatScheduleItem _ _ ScheduleItem { serviceName = sn, departureTime = dt, scheduleType = CANCELED } =
  sn ++ " (" ++ show dt ++ " !CANC!) "
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
