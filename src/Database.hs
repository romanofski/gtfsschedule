{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE PatternGuards              #-}
module Database where

import Data.Time.LocalTime ( TimeOfDay(..)
                           , timeOfDayToTime
                           , timeToTimeOfDay)
import Data.Time.Clock (secondsToDiffTime, getCurrentTime, UTCTime(..))
import Data.Time.Calendar ( Day(..))
import Data.Time.Format ( formatTime
                        , defaultTimeLocale)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (runResourceT, MonadResource, ResourceT)
import Control.Monad.Trans.Resource (MonadBaseControl)
import Control.Monad.Logger (runNoLoggingT, runStderrLoggingT, MonadLoggerIO, NoLoggingT, LoggingT)
import Database.Persist.TH
import Database.Esqueleto
import Data.List (stripPrefix)
import System.Environment.XDG.BaseDir (getUserDataFile)
import qualified Database.Persist.Sqlite as Sqlite
import qualified Data.Text as T


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ImportStarted
    upid UpdateProcessId
ImportFinished
    upid UpdateProcessId
UpdateProcess
    when Day
    status Bool
StopTime
    tripId String
    arrivalTime TimeOfDay
    departureTime TimeOfDay
    stopId String
    stopSequence Int
    pickupType Int Maybe
    dropOffType Int Maybe
    deriving Show Eq
Trip
    routeId String
    serviceId String
    tripId String
    headsign String Maybe
    directionId Bool Maybe
    blockId String Maybe
    shapeId String Maybe
    deriving Show Eq
Calendar
    serviceId String
    monday Bool
    tuesday Bool
    wednesday Bool
    thursday Bool
    friday Bool
    saturday Bool
    sunday Bool
    startDate Day
    endDate Day
Stop
    stopId String
    code String Maybe
    name String
    desc String Maybe
    lat Double
    lon Double
    zoneId String Maybe
    url String Maybe
    locationType Int Maybe
    parentStation String Maybe
Route
    routeId String
    shortName String
    longName String
    desc String Maybe
    type String
    url String Maybe
    color String Maybe
    textColor String Maybe
    deriving Show Eq
|]
-- ^-- TODO
--  RouteType could be enumeration

-- | Location of the Sqlite database file in the users home
--
userDatabaseFile ::
  IO String
userDatabaseFile = getUserDataFile "gtfs" "gtfs.sqlite"

weekdayToSQLExp ::
  String
  -> EntityField Calendar Bool
weekdayToSQLExp weekday
  | Just _ <- stripPrefix "Monday" weekday = CalendarMonday
  | Just _ <- stripPrefix "Tuesday" weekday = CalendarTuesday
  | Just _ <- stripPrefix "Wednesday" weekday = CalendarWednesday
  | Just _ <- stripPrefix "Thursday" weekday = CalendarThursday
  | Just _ <- stripPrefix "Friday" weekday = CalendarFriday
  | Just _ <- stripPrefix "Saturday" weekday = CalendarSaturday
  | Just _ <- stripPrefix "Sunday" weekday = CalendarSunday
  | otherwise = error "That should never happen"

getStopID ::
  (MonadLoggerIO m, MonadResource m)
  => String
  -> ReaderT Sqlite.SqlBackend m [Value String]
getStopID stopC = select $ from $ \s -> do
  where_ (s ^. StopCode ==. val (Just stopC))
  return (s ^. StopStopId)

-- | Returns next possible departures
--
-- TODO: limit is currently hard coded
--
getNextDepartures ::
  (MonadLoggerIO m, MonadResource m)
  => String
  -> TimeOfDay
  -> Day
  -> ReaderT Sqlite.SqlBackend m [(Sqlite.Entity StopTime, Sqlite.Entity Trip, Sqlite.Entity Route)]
getNextDepartures stopID now nowDate = select $ from $ \(st, t, c, s, r) -> do
  where_ (
    st ^. StopTimeTripId ==. t ^. TripTripId &&.
    t ^. TripRouteId ==. r ^. RouteRouteId &&.
    t ^. TripServiceId ==. c ^. CalendarServiceId &&.
    st ^. StopTimeStopId ==. s ^. StopStopId &&.
    s ^. StopStopId ==. val stopID &&.
    st ^. StopTimeDepartureTime >. val earliest &&.
    st ^. StopTimeDepartureTime <. val latest &&.
    c ^. weekdaySqlExp &&.
    c ^. CalendarEndDate >=. val nowDate &&.
    c ^. CalendarStartDate <=. val nowDate
    )
  orderBy [asc (st ^. StopTimeDepartureTime)]
  limit 3
  return (st, t, r)
  where earliest = timeToTimeOfDay $ timeOfDayToTime now - secondsToDiffTime 60
        latest = timeToTimeOfDay $ timeOfDayToTime now + secondsToDiffTime 60 * 30
        weekday = formatTime defaultTimeLocale "%A" nowDate
        weekdaySqlExp = weekdayToSQLExp weekday

getDatabaseInfo ::
  (MonadLoggerIO m, MonadResource m)
  => ReaderT Sqlite.SqlBackend m [(Sqlite.Entity UpdateProcess)]
getDatabaseInfo = select $ from $ \(uf, up) -> do
  where_(
    uf ^. ImportFinishedUpid ==. up ^. UpdateProcessId &&.
    up ^. UpdateProcessStatus ==. val True)
  limit 1
  return up

getLastUpdatedDatabase ::
  T.Text
  -> IO Day
getLastUpdatedDatabase connstr = runDBWithoutLogging connstr $ do
  dbinfo <- getDatabaseInfo
  let lastupdated = updateProcessWhen . entityVal <$> dbinfo
  return $ head lastupdated


data UpdateType = Started
                | Finished


-- | prepares new database which is used if we're importing a new dataset
prepareDatabaseForUpdate ::
  (MonadIO m, MonadResource m)
  => UpdateType
  -> ReaderT Sqlite.SqlBackend m ()
prepareDatabaseForUpdate updateType = do
  now <- liftIO getCurrentTime
  upKey <- insert $ UpdateProcess { updateProcessWhen = utctDay now, updateProcessStatus = True }
  case updateType of
    Started -> insert_ $ ImportStarted { importStartedUpid = upKey }
    Finished -> insert_ $ ImportFinished { importFinishedUpid = upKey }

runDBWithLogging ::
  (MonadIO m, MonadBaseControl IO m)
  => T.Text
  -> SqlPersistT (LoggingT (ResourceT m)) a -> m a
runDBWithLogging dbName = runResourceT . runStderrLoggingT . Sqlite.withSqliteConn dbName . Sqlite.runSqlConn

runDBWithoutLogging ::
  (MonadIO m, MonadBaseControl IO m)
  => T.Text
  -> SqlPersistT (NoLoggingT (ResourceT m)) a -> m a
runDBWithoutLogging dbName = runResourceT . runNoLoggingT . Sqlite.withSqliteConn dbName . Sqlite.runSqlConn






