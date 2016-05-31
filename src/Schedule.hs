-- | the GTFS schedule
module Schedule where

import Data.Time.LocalTime ( utcToLocalTime
                           , LocalTime(..)
                           , TimeOfDay(..)
                           , timeOfDayToTime
                           , timeToTimeOfDay
                           , TimeZone)
import Data.Time (getCurrentTime, getCurrentTimeZone)
import Data.Time.Clock (secondsToDiffTime, DiffTime, UTCTime)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import qualified Database as DB
import qualified Data.Text as T
import qualified Database.Persist.Sqlite as Sqlite


-- | A scheduled service
data ScheduleItem = ScheduleItem { tripId :: String
                                 , stopId :: String
                                 , serviceName :: String
                                 , scheduledDepartureTime :: TimeOfDay
                                 , departureDelay :: Integer
                                 , departureTime :: TimeOfDay
                                 }

data Schedule a = Schedule [a]


makeSchedule ::
  [(Sqlite.Entity DB.StopTime, Sqlite.Entity DB.Trip)]
  -> [ScheduleItem]
makeSchedule stops = (\(x, y) -> makeItem (Sqlite.entityVal x, Sqlite.entityVal y)) <$> stops
  where
    makeItem (st, t) = ScheduleItem { tripId = DB.tripTripId t
                                    , stopId = DB.stopTimeStop st
                                    , serviceName = fromMaybe (DB.tripTripId t) $ DB.tripHeadsign t
                                    , scheduledDepartureTime = DB.stopTimeDepartureTime st
                                    , departureDelay = 0
                                    , departureTime = DB.stopTimeDepartureTime st
                                    }


getSchedule ::
  String
  -> String
  -> Integer
  -> IO [ScheduleItem]
getSchedule sqliteDBFilepath sID delay = DB.runDBWithoutLogging (T.pack sqliteDBFilepath) $ do
  t <- liftIO getCurrentTime
  tz <- liftIO getCurrentTimeZone
  let lday = localDay $ utcToLocalTime tz t
  let earliestTime = calculateEarliestDepartureTime t tz (delayAsDiffTime delay)
  stops <- DB.getNextDepartures sID earliestTime lday
  return $ makeSchedule stops

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
  [ScheduleItem]
  -> IO ()
printSchedule xs = do
  t <- getCurrentTime
  tz <- getCurrentTimeZone
  putStr $ concat $ printScheduleItem t tz <$> xs

printScheduleItem ::
  UTCTime
  -> TimeZone
  -> ScheduleItem
  -> String
printScheduleItem t tz item =
  delayIndicator ++ serviceName item ++ " " ++ show (minutesToDeparture item lTimeOfDay) ++ " min (" ++ show (departureTime item) ++ schedDepTime ++ ") "
    where
      delayIndicator = if departureDelay item > 0 then "!" else ""
      lTimeOfDay = localTimeOfDay $ utcToLocalTime tz t
      schedDepTime = if departureDelay item > 0
                     then " - " ++ show (departureDelay item) ++ "s"
                     else ""

minutesToDeparture ::
  ScheduleItem
  -> TimeOfDay
  -> Integer
minutesToDeparture x now = round $ toRational (timeOfDayToTime depTime - secondsToDeparture now delay) / 60
  where
    delay = delayAsDiffTime $ departureDelay x
    depTime = departureTime x

secondsToDeparture ::
  TimeOfDay
  -> DiffTime
  -> DiffTime
secondsToDeparture now delay = timeOfDayToTime now + delay
