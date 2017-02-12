{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{- | This module provides schedule information. The information is primarily
retrieved from the static schedule (e.g. from the database), but is updated with
realtime information.
-}
module GTFS.Schedule
       (ScheduleState(..), ScheduleItem(..), TimeSpec(..),
        StopWithWalktime, ScheduleConfig(..), getSchedule, getSchedulesByWalktime,
        getTimeSpecFromNow, printSchedule,
        minutesToDeparture, secondsToDeparture, sortSchedules,
        bumOffSeatTime, getCurrentTimeOfDay, humanReadableDelay,
        defaultScheduleConfig, defaultScheduleItemTemplate)
       where

import qualified GTFS.Database as DB

import Control.Applicative ((<$>), pure)
import Data.Traversable (traverse)
import Data.List (sortBy)
import Data.Ord (comparing)

import Data.Time.LocalTime ( utcToLocalTime
                           , LocalTime(..)
                           , TimeOfDay(..)
                           , timeOfDayToTime
                           , timeToTimeOfDay
                           , TimeZone)
import Data.Time.Calendar (Day)
#if MIN_VERSION_time(1, 5, 0)
import Data.Time.Format (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif
import Data.Time.Format (formatTime)
import Data.Time (getCurrentTime, getCurrentTimeZone)
import Data.Time.Clock (secondsToDiffTime, DiffTime, UTCTime)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.Esqueleto (unValue)
import qualified Database.Persist.Sqlite as Sqlite
import Text.StringTemplate (newSTMP, render, setManyAttrib)


type StopWithWalktime = (String, Integer)

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

-- | Returns several schedules for given stops with walk time
getSchedulesByWalktime ::
  String  -- ^ database file path
  -> Integer  -- ^ limit
  -> [StopWithWalktime]
  -> IO [(Integer, [ScheduleItem])]
getSchedulesByWalktime fp l stops = do
  schedules <- traverse (go fp) stops
  pure schedules
    where go filep (sID, walkDelay) = do
            timespec <- getTimeSpecFromNow walkDelay
            schedule <- getSchedule filep sID timespec l
            pure (walkDelay, schedule)

-- | Return the next services which are due in the next couple of minutes
getSchedule ::
  String  -- ^ database file path
  -> String  -- ^ stop code
  -> TimeSpec
  -> Integer  -- ^ limit
  -> IO [ScheduleItem]
getSchedule sqliteDBFilepath sCode timespec l = nextDepartures (T.pack sqliteDBFilepath) sCode timespec l

-- | Given a list of schedules paired with an associated walk delay,
-- sort the schedule items by bum-off-seat time.
--
sortSchedules :: [(Integer, [ScheduleItem])] -> [(Integer, ScheduleItem)]
sortSchedules xxs =
  sortBy (comparing bumOffSeatTime) $ xxs >>= (\(d,xs) -> fmap (d,) xs)

bumOffSeatTime :: (Integer, ScheduleItem) -> DiffTime
bumOffSeatTime (d, item) = timeOfDayToTime (departureTime item) - secondsToDiffTime (d * 60)

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

printSchedule
    :: [(Integer, ScheduleItem)]
    -> ScheduleConfig
    -> IO ()
printSchedule [] _ =
    let timespanInMin = show $ round (DB.queryTimeWindowLatest DB.nextServicesTimeWindow / 60)
    in putStr $ "No services for the next " ++ timespanInMin ++ "min"
printSchedule xs cfg =
    putStr $
    concat $
    (\(d,x) ->
          defaultScheduleItemFormatter cfg d x) <$>
    xs

data ScheduleConfig = ScheduleConfig
    { scheduleTimeOfDay :: TimeOfDay
    , scheduleItemTemplate :: T.Text
    }

defaultScheduleItemTemplate :: T.Text
defaultScheduleItemTemplate = "$delayIndicator$$serviceName$ $minutesToDeparture$min $departureTime$ $scheduledDepartureTime$ $scheduleTypeDiff$"

defaultScheduleConfig :: TimeOfDay -> ScheduleConfig
defaultScheduleConfig tod =
  ScheduleConfig
    { scheduleTimeOfDay = tod
    , scheduleItemTemplate = defaultScheduleItemTemplate
    }

defaultScheduleItemFormatter :: ScheduleConfig
                             -> Integer
                             -> ScheduleItem
                             -> String
defaultScheduleItemFormatter cfg walkDelay si = render attributesToTemplate
  where
    attributesToTemplate =
        setManyAttrib
            [ ( "delayIndicator"
              , if departureDelay si /= 0
                    then Just "!"
                    else Just "")
            , ("serviceName", Just $ serviceName si)
            , ( "minutesToDeparture"
              , Just $ show
                    (minutesToDeparture si (scheduleTimeOfDay cfg) - walkDelay))
            , ("departureTime", Just $ show (departureTime si))
            , ("scheduledDepartureTime", humanReadableDelay si)
            , ("scheduleType", Just $ show $ scheduleType si)
            , ("scheduleTypeDiff", scheduleTypeWithoutDefault si)
            ]
            (newSTMP (T.unpack $ scheduleItemTemplate cfg))

scheduleTypeWithoutDefault :: ScheduleItem -> Maybe String
scheduleTypeWithoutDefault (ScheduleItem { scheduleType = SCHEDULED }) = Nothing
scheduleTypeWithoutDefault s = Just $ show $ scheduleType s

-- | Converts the delay to a more readable format
--
-- We convert the delay to a current time from midnight and then use the common
-- formatTime function to produce the user friendly format. That doesn't work
-- for negative values (in cases the service is running ahead of it's schedule).
-- To work with negative values, we use the absolute value instead of a prefixed
-- value which always produces the right time from midnight.
--
humanReadableDelay :: ScheduleItem -> Maybe String
humanReadableDelay x
  | depDelay == 0 = Nothing
  | depDelay < 60 = Just $ formatTime defaultTimeLocale (prefix ++ "%Ss") t
  | depDelay < 60 * 60 = Just $ formatTime defaultTimeLocale (prefix ++ "%M:%S") t
  | otherwise = Just $ formatTime defaultTimeLocale (prefix ++ "%k:%M:%S") t
  where
    t = timeToTimeOfDay $ secondsToDiffTime $ depDelay
    depDelay = abs $ departureDelay x
    prefix =
        if departureDelay x <= 0
            then "-"
            else "+"

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
  -> Integer  -- ^ limit
  -> IO [ScheduleItem]
nextDepartures connstr sCode (TimeSpec earliest day) l = DB.runDBWithoutLogging connstr $ do
  sID <- DB.getStopID sCode
  stops <- DB.getNextDepartures (firstStopID sID) earliest day l
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
