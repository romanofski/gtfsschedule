{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TupleSections #-}

{- | This module provides schedule information. The information is primarily retrieved from the static schedule (e.g. from the database), but is updated with realtime information.
-}
module GTFS.Schedule
       (ScheduleState(..), ScheduleItem(..), TimeSpec(..), getSchedule,
        getTimeSpecFromNow, printSchedule, formatScheduleItem,
        minutesToDeparture, secondsToDeparture, sortSchedules,
        getCurrentTimeOfDay)
       where

import qualified GTFS.Database as DB

import Data.Functor ((<$>))
import Data.List (sortBy)
import Data.Ord (comparing)

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
data ScheduleState
    = CANCELED
    | ADDED
    | SCHEDULED
    deriving (Show,Eq,Ord)

-- | One entity giving information about the departure of a service
data ScheduleItem = ScheduleItem
    { tripId :: String
    , stopId :: String
    , serviceName :: String  -- ^ short service name
    , scheduledDepartureTime :: TimeOfDay  -- ^ the time this service departure was originally scheduled
    , departureDelay :: Integer  -- ^ delay retrieved from the realtime feed if available
    , departureTime :: TimeOfDay  -- ^ departure time including the realtime update if available
    , scheduleType :: ScheduleState
    } deriving (Show,Eq,Ord)

-- | A specific point in time from when we want to calculate the next departing
--   services
data TimeSpec = TimeSpec TimeOfDay Day


-- | Return the next services which are due in the next couple of minutes
getSchedule ::
  String  -- ^ database file path
  -> String  -- ^ stop code
  -> TimeSpec
  -> IO [ScheduleItem]
getSchedule sqliteDBFilepath sCode timespec = nextDepartures (T.pack sqliteDBFilepath) sCode timespec

-- | Given a list of schedules paired with an associated walk delay,
-- sort the schedule items by bum-off-seat time.
--
sortSchedules :: [(Integer, [ScheduleItem])] -> [(Integer, ScheduleItem)]
sortSchedules xxs =
  sortBy (comparing f) $ xxs >>= (\(d,xs) -> fmap (d,) xs)
  where
    f (d, item) = timeOfDayToTime (departureTime item) - secondsToDiffTime (d * 60)

-- | Create a specific point in time from the current time/date
getTimeSpecFromNow ::
  Integer
  -> IO TimeSpec
getTimeSpecFromNow delay = do
      t <- getCurrentTime
      tz <- getCurrentTimeZone
      let lday = localDay $ utcToLocalTime tz t
      let earliestTime = calculateEarliestDepartureTime t tz (delayAsDiffTime delay)
      return $ TimeSpec earliestTime lday


-- | Returns current time of day
--
getCurrentTimeOfDay :: IO TimeOfDay
getCurrentTimeOfDay = do
  t <- getCurrentTime
  tz <- getCurrentTimeZone
  return $ localTimeOfDay $ utcToLocalTime tz t

printSchedule :: [(Integer, ScheduleItem)] -> TimeOfDay -> IO ()
printSchedule [] _ =
    let timespanInMin = show $ round (DB.queryTimeWindowLatest DB.nextServicesTimeWindow / 60)
    in putStr $ "No services for the next " ++ timespanInMin ++ "min"
printSchedule xs tod = do
    putStr $
        concat $
        (\(d,x) ->
              formatScheduleItem tod d x) <$>
        xs

-- | Format a schedule item in a user friendly way for printing
formatScheduleItem ::
  TimeOfDay  -- ^ current time (typically invokation of the program)
  -> Integer  -- ^ delay in seconds which is subtracted from current time
  -> ScheduleItem  -- ^ item to format
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

-- | calculates how many minutes we have before the service departs
minutesToDeparture ::
  ScheduleItem
  -> TimeOfDay  -- ^ current time
  -> Integer
minutesToDeparture x now = round $ toRational (depTimeInSeconds - nowInSeconds) / 60
  where
    nowInSeconds = timeOfDayToTime now
    depTimeInSeconds = timeOfDayToTime $ departureTime x

-- | conversion function to calculate the seconds until departure
secondsToDeparture ::
  TimeOfDay  -- ^ current time
  -> DiffTime  -- ^ delay to add to current time (e.g. how long it takes us to walk to the stop)
  -> DiffTime
secondsToDeparture now delay = timeOfDayToTime now + delay


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
