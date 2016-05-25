-- | the GTFS schedule
module Schedule
    ( printSchedule
    , getSchedule
    ) where

import Data.Time.LocalTime ( utcToLocalTime
                           , LocalTime(..)
                           , timeOfDayToTime
                           , timeToTimeOfDay)
import Data.Time (getCurrentTime, getCurrentTimeZone)
import Data.Time.Clock (secondsToDiffTime)
import Control.Monad.IO.Class (liftIO)
import qualified Database as DB
import qualified Data.Text as T
import qualified Database.Persist.Sqlite as Sqlite


-- | prints list of StopTimes as schedule
--
printSchedule ::
  String
  -> String
  -> Integer
  -> IO ()
printSchedule sqliteDBFilepath sID delay = do
  t <- liftIO getCurrentTime
  tz <- liftIO getCurrentTimeZone
  let delaySeconds = secondsToDiffTime (delay * 60)
  let (LocalTime _ lTimeOfDay) = utcToLocalTime tz t
  stops <- getSchedule sqliteDBFilepath sID delay
  liftIO $ putStr $ DB.printStopTimesAsSchedule lTimeOfDay delaySeconds stops

getSchedule ::
  String
  -> String
  -> Integer
  -> IO [(Sqlite.Entity DB.StopTime, Sqlite.Entity DB.Trip)]
getSchedule sqliteDBFilepath sID delay = DB.runDBWithoutLogging (T.pack sqliteDBFilepath) $ do
  t <- liftIO getCurrentTime
  tz <- liftIO getCurrentTimeZone
  let delaySeconds = secondsToDiffTime (delay * 60)
  let (LocalTime lDay lTimeOfDay) = utcToLocalTime tz t
  let lTimeWithDelay = timeToTimeOfDay $ timeOfDayToTime lTimeOfDay + delaySeconds
  DB.getNextDepartures sID lTimeWithDelay lDay
