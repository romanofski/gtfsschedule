{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-
Copyright (C) - 2017-2020 Róman Joost <roman@bromeco.de>

This file is part of gtfsschedule.

gtfsschedule is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

gtfsschedule is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with gtfsschedule.  If not, see <http://www.gnu.org/licenses/>.
-}
{- | Database integration with Sqlite

This module provides basic functions to interact with the static database imported in Sqlite.

-}
module GTFS.Database
       (ImportStarted(..), ImportStartedId, ImportFinished(..),
        ImportFinishedId, StopTime(..), StopTimeId, Trip(..), TripId,
        Calendar(..), CalendarId, Stop(..), StopId, Route(..), RouteId,
        UpdateType(..), QueryTimeWindow(..), StopSearchResult(..),
        migrateAll, userDatabaseFile, getStopID, getNextDepartures,
        isNotFinalStop, getLastUpdatedDatabase, prepareDatabaseForUpdate,
        addDatabaseIndices, prepareStmt, rawInsert, runDBWithLogging,
        runDBWithoutLogging, nextServicesTimeWindow, searchStopCode)
       where

import           Data.Time.Calendar             (Day (..))
import           Data.Time.Clock                (DiffTime, UTCTime (..),
                                                 getCurrentTime,
                                                 secondsToDiffTime)
import           Data.Time.LocalTime            (TimeOfDay (..),
                                                 timeOfDayToTime,
                                                 timeToTimeOfDay)
import           Data.Time.Format               (defaultTimeLocale, formatTime)

import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Logger           (LoggingT, MonadLoggerIO,
                                                 NoLoggingT, runNoLoggingT,
                                                 runStderrLoggingT)
import           Control.Monad.IO.Unlift        (MonadUnliftIO)
import           Control.Monad.Trans.Reader     (ReaderT, ask)
import           Control.Monad.Trans.Resource   (MonadResource, ResourceT,
                                                 runResourceT)
import           Control.Monad.Trans.Control    (MonadBaseControl)
import           Data.Int                       (Int64)
import           Data.List                      (stripPrefix)
import           Data.Maybe                     (fromMaybe)
import qualified Data.Text                      as T
import           Database.Esqueleto
import qualified Database.Persist.Sqlite        as Sqlite
import           Database.Persist.TH
import           System.Environment.XDG.BaseDir (getUserDataFile)

{-# ANN module ("HLint: ignore Redundant ^." :: String) #-}

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

-- | Returns possible stop codes matching
--
data StopSearchResult = StopSearchResult
    { resultStopName :: String
    , resultStopcode :: String
    , resultStopURL  :: Maybe String
    } deriving (Show)

searchStopCode :: T.Text -> String -> IO [StopSearchResult]
searchStopCode dbfile s =
    runDBWithoutLogging dbfile $
    do records <- searchStopCodeByName s
       return $
           (\e ->
                 StopSearchResult
                 { resultStopName = stopName e
                 , resultStopcode = fromMaybe (stopStopId e) (stopCode e)
                 , resultStopURL = stopUrl e
                 }) .
           entityVal <$>
           records

searchStopCodeByName
    :: (MonadLoggerIO m, MonadResource m)
    => String -> ReaderT Sqlite.SqlBackend m [Sqlite.Entity Stop]
searchStopCodeByName str =
    select $
    from $
    \s ->
         do where_ (s ^. StopName `like` (%) ++. val str ++. (%))
            return s

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

isNotFinalStop ::
  (MonadLoggerIO m, MonadResource m)
  => (Sqlite.Entity StopTime, Sqlite.Entity Trip, Sqlite.Entity Route, Sqlite.Entity Stop)
  -> ReaderT Sqlite.SqlBackend m Bool
isNotFinalStop (st,_,_,_) = do
    stoptimes <-
        select $
        from $
        \stoptime ->
             do where_
                    (stoptime ^. StopTimeTripId ==.
                     val (stopTimeTripId $ Sqlite.entityVal st) &&.
                     stoptime ^.
                     StopTimeStopSequence >.
                     val (stopTimeStopSequence $ entityVal st))
                return stoptime
    return $ not (null stoptimes)

-- | Returns the stop id for a given stop code
getStopID ::
  (MonadLoggerIO m, MonadResource m)
  => String  -- ^ stop code
  -> ReaderT Sqlite.SqlBackend m [Value String]
getStopID stopC = select $ from $ \s -> do
  where_ (s ^. StopCode ==. val (Just stopC))
  return (s ^. StopStopId)

-- | A window in between services are queried
--
data QueryTimeWindow = QueryTimeWindow
    { queryTimeWindowEarliest :: DiffTime
    , queryTimeWindowLatest   :: DiffTime
    } deriving (Show)

nextServicesTimeWindow :: QueryTimeWindow
nextServicesTimeWindow = QueryTimeWindow (secondsToDiffTime 60) (secondsToDiffTime 60 * 30)

-- | Returns next possible departures
--
-- Note: Even tho the limit is variable, the query enforces a hard ceiling of 20
-- records per query. Reason being from the current user cases, there shouldn't
-- be a need for querying more than 20. This provides the benefit that users
-- can't query too many records by mistake.
--
getNextDepartures
    :: (MonadLoggerIO m, MonadResource m)
    => String  -- ^ stop id
    -> TimeOfDay  -- ^ current time
    -> Day  -- ^ current date
    -> Integer  -- ^ limit
    -> ReaderT Sqlite.SqlBackend m [(Sqlite.Entity StopTime, Sqlite.Entity Trip, Sqlite.Entity Route, Sqlite.Entity Stop)]
getNextDepartures stopID now nowDate l = select $ from $ \(st, t, c, s, r) -> do
  where_ (
    st ^. StopTimeTripId ==. t ^. TripTripId &&.
    t ^. TripRouteId ==. r ^. RouteRouteId &&.
    t ^. TripServiceId ==. c ^. CalendarServiceId &&.
    st ^. StopTimeStopId ==. s ^. StopStopId &&.
    s ^. StopStopId ==. val stopID &&.
    st ^. StopTimeDepartureTime >. val earliest &&.
    c ^. weekdaySqlExp &&.
    c ^. CalendarEndDate >=. val nowDate &&.
    c ^. CalendarStartDate <=. val nowDate
    )
  orderBy [asc (st ^. StopTimeDepartureTime)]
  limit (fromInteger (min l 20))
  return (st, t, r, s)
  where earliest = timeToTimeOfDay $ timeOfDayToTime now - queryTimeWindowEarliest nextServicesTimeWindow
        weekday = formatTime defaultTimeLocale "%A" nowDate
        weekdaySqlExp = weekdayToSQLExp weekday

getDatabaseInfo ::
  (MonadLoggerIO m, MonadResource m)
  => ReaderT Sqlite.SqlBackend m [Sqlite.Entity UpdateProcess]
getDatabaseInfo = select $ from $ \(uf, up) -> do
  where_(
    uf ^. ImportFinishedUpid ==. up ^. UpdateProcessId &&.
    up ^. UpdateProcessStatus ==. val True)
  limit 1
  return up

-- | Returns information about the last time the database was updated, which is
-- needed for determining if the static dataset has been updated
--
getLastUpdatedDatabase ::
  T.Text  -- ^ database file path
  -> IO Day
getLastUpdatedDatabase connstr = runDBWithoutLogging connstr $ do
  dbinfo <- getDatabaseInfo
  let lastupdated = updateProcessWhen . entityVal <$> dbinfo
  return $ head lastupdated


-- | Datatype to symbolize the update process from started to finished
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

-- | Add needed database indices for a speedy lookup
addDatabaseIndices ::
  (MonadIO m, MonadResource m)
  => ReaderT Sqlite.SqlBackend m ()
addDatabaseIndices = do
  mapM_ (\x -> Sqlite.rawExecute (T.pack x) []) indices
    where indices = [ "create index stop_time_index ON stop_time (stop_id);"
                    , "create index trip_index on trip (trip_id, route_id, service_id);"
                    , "create index route_index on route (route_id);"
                    , "create index calendar_index on calendar (service_id);"
                    , "create index stop_index on stop (stop_id, code)"
                    ]

-- | Low-level sqlite preparation of statements
prepareStmt ::
  (MonadIO m)
  => T.Text
  -> ReaderT Sqlite.SqlBackend m Sqlite.Statement
prepareStmt sql = do
  conn <- persistBackend <$> ask
  liftIO $ Sqlite.connPrepare conn sql

-- | Low-level sqlite insert of a prepared statement
rawInsert ::
  (MonadIO m, MonadResource m)
  => Sqlite.Statement
  -> [Sqlite.PersistValue]
  -> ReaderT Sqlite.SqlBackend m Int64
rawInsert stmt vals = do
  res <- liftIO $ Sqlite.stmtExecute stmt vals
  liftIO $ Sqlite.stmtReset stmt
  return res

runDBWithLogging ::
  (MonadUnliftIO m, MonadBaseControl IO m)
  => T.Text
  -> SqlPersistT (LoggingT (ResourceT m)) a -> m a
runDBWithLogging dbName = runResourceT . runStderrLoggingT . Sqlite.withSqliteConn dbName . Sqlite.runSqlConn

runDBWithoutLogging ::
  (MonadUnliftIO m, MonadBaseControl IO m)
  => T.Text
  -> SqlPersistT (NoLoggingT (ResourceT m)) a -> m a
runDBWithoutLogging dbName = runResourceT . runNoLoggingT . Sqlite.withSqliteConn dbName . Sqlite.runSqlConn
