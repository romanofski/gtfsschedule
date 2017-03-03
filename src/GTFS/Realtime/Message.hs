{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{- | Module to parse the real time feed for real time updates

This module uses protocol buffers to parse the feedmessage in order to update
the schedule data.

See also: https://developers.google.com/transit/gtfs-realtime/reference/
-}
module GTFS.Realtime.Message
       (updateSchedule, updateSchedulesWithRealtimeData,
        departureTimeWithDelay, getTripUpdates, FM.FeedMessage)
       where

import GTFS.Schedule (ScheduleItem(..), ScheduleState(..), Stop(..), VehicleInformation(..), secondsToDeparture)

import Control.Applicative (pure, (<$>))
import Prelude hiding (mapM)
import Data.Traversable (mapM)

import Network.HTTP.Conduit (simpleHttp)
import Text.ProtocolBuffers (messageGet)
import qualified Data.Text as T

import Com.Google.Transit.Realtime.TripUpdate.StopTimeEvent (StopTimeEvent(..), delay)
import Com.Google.Transit.Realtime.TripDescriptor (trip_id, TripDescriptor(..))
import qualified Com.Google.Transit.Realtime.TripDescriptor.ScheduleRelationship as TripSR
import qualified Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate.ScheduleRelationship as StopTUSR
import qualified Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate as STU
import qualified Com.Google.Transit.Realtime.VehiclePosition as VP
import qualified Com.Google.Transit.Realtime.VehiclePosition.CongestionLevel as CL
import qualified Com.Google.Transit.Realtime.VehiclePosition.OccupancyStatus as O
import qualified Com.Google.Transit.Realtime.FeedMessage as FM
import qualified Com.Google.Transit.Realtime.TripUpdate as TU
import qualified Com.Google.Transit.Realtime.FeedEntity as FE

import Text.ProtocolBuffers (utf8)
import Text.ProtocolBuffers.Basic (Utf8)
import qualified Text.ProtocolBuffers.Header as P'
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import Data.Time.LocalTime (timeToTimeOfDay, TimeOfDay)
import Data.Time.Clock (secondsToDiffTime)
import Data.Foldable (find)
import qualified Data.Map.Lazy as Map
import Control.Monad (mfilter)
import Control.Monad.State (State, execState, get, put)


type Schedule = Map.Map String ScheduleItem
-- | Updates the schedule with realtime information from the GTFS feed
--
updateSchedulesWithRealtimeData ::
  Maybe T.Text
  -> [ScheduleItem]
  -> IO [ScheduleItem]
updateSchedulesWithRealtimeData Nothing schedules = pure schedules
updateSchedulesWithRealtimeData (Just url) schedules = do
    bytes <- simpleHttp (T.unpack url)
    case messageGet bytes of
        Left err -> do
            print $ "Error occurred decoding feed: " ++ err
            pure schedules
        Right (fm,_) -> do
            pure $ updateSchedule schedules getVehiclePositions fm >> updateSchedule schedules getTripUpdates fm

-- | Updates schedule with trip updates given by feed
--
updateSchedule
    :: ForFeedElement e
    => [ScheduleItem]
    -> (FM.FeedMessage -> P'.Seq e)
    -> FM.FeedMessage
    -> [ScheduleItem]
updateSchedule schedule getter fm = Map.elems $ execState (mapM updateFeedElement $ getter fm) scheduleMap
  where
    scheduleMap = Map.fromList $ toMap <$> schedule
    toMap x = (tripId x, x)

-- | calculate the new departure time with a delay from the real time update
departureTimeWithDelay ::
  TimeOfDay
  -> Integer
  -> TimeOfDay
departureTimeWithDelay depTime d = timeToTimeOfDay $ secondsToDeparture depTime (secondsToDiffTime d)

getTripUpdates ::
  FM.FeedMessage
  -> P'.Seq TU.TripUpdate
getTripUpdates fm = (`P'.getVal` FE.trip_update) <$> entity
  where entity = P'.getVal fm FM.entity

getVehiclePositions ::
  FM.FeedMessage
  -> P'.Seq VP.VehiclePosition
getVehiclePositions fm = (`P'.getVal` FE.vehicle) <$> (P'.getVal fm FM.entity)

class ForFeedElement e where
  getTripID :: e -> String
  getTripID x = utf8ToString $ P'.getVal (getTripDescriptor x) trip_id

  getTripDescriptor :: e -> TripDescriptor
  updateScheduleItem :: e -> String -> ScheduleItem -> Maybe ScheduleItem

instance ForFeedElement TU.TripUpdate where
    getTripDescriptor x = P'.getVal x TU.trip
    updateScheduleItem TU.TripUpdate{TU.trip = TripDescriptor{schedule_relationship = Just TripSR.CANCELED}} k item =
        Just
            ScheduleItem
            { tripId = k
            , stop = stop item
            , serviceName = serviceName item
            , scheduledDepartureTime = scheduledDepartureTime item
            , departureDelay = 0
            , departureTime = departureTime item
            , scheduleType = CANCELED
            , scheduleItemVehicleInformation = scheduleItemVehicleInformation item
            }
    updateScheduleItem tu k item = do
        stu <- findStopTimeUpdate (stop item) (getStopTimeUpdates tu)
        Just
            ScheduleItem
            { tripId = k
            , stop = stop item
            , serviceName = serviceName item
            , scheduledDepartureTime = scheduledDepartureTime item
            , departureDelay = getDepartureDelay stu
            , departureTime = departureTimeWithDelay
                  (scheduledDepartureTime item)
                  (getDepartureDelay stu)
            , scheduleType = scheduleTypeForStop stu
            , scheduleItemVehicleInformation = scheduleItemVehicleInformation item
            }

instance ForFeedElement VP.VehiclePosition where
  getTripDescriptor x = P'.getVal x VP.trip
  updateScheduleItem vp k item = Just ScheduleItem
            { tripId = k
            , stop = stop item
            , serviceName = serviceName item
            , scheduledDepartureTime = scheduledDepartureTime item
            , departureDelay = departureDelay item
            , departureTime = departureTime item
            , scheduleType = scheduleType item
            , scheduleItemVehicleInformation = makeVehicleInformation vp
            }


updateFeedElement ::
  ForFeedElement e => e
  -> State Schedule ()
updateFeedElement x = do
    m <- get
    let (_,map') =
            Map.updateLookupWithKey (updateScheduleItem x) (getTripID x) m
    put map'

makeVehicleInformation ::
  VP.VehiclePosition
  -> VehicleInformation
makeVehicleInformation vp = let congestionl = fromEnum (P'.getVal vp VP.congestion_level)
                                c_percentage = (congestionl * 100) `div` fromEnum (maxBound :: CL.CongestionLevel)
                                occupancys = fromEnum (P'.getVal vp VP.occupancy_status)
                                o_percentage = (occupancys * 100) `div` fromEnum (maxBound :: O.OccupancyStatus)
                            in VehicleInformation (Just c_percentage) (Just o_percentage)

-- | helper to set the appropriate schedule type if the service will skip this stop
--
scheduleTypeForStop ::
  STU.StopTimeUpdate
  -> ScheduleState
scheduleTypeForStop STU.StopTimeUpdate { STU.schedule_relationship = Just StopTUSR.SKIPPED } = CANCELED
scheduleTypeForStop _ = SCHEDULED

getStopTimeUpdates ::
  TU.TripUpdate
  -> P'.Seq STU.StopTimeUpdate
getStopTimeUpdates msg = P'.getVal msg TU.stop_time_update

findStopTimeUpdate ::
  Stop
  -> P'.Seq STU.StopTimeUpdate
  -> Maybe STU.StopTimeUpdate
findStopTimeUpdate s = find (\x -> stopTimeUpdateStopID x == stopIdentifier s)

stopTimeUpdateStopID ::
  STU.StopTimeUpdate
  -> String
stopTimeUpdateStopID msg = utf8ToString $ P'.getVal msg STU.stop_id

getDepartureDelay ::
  STU.StopTimeUpdate
  -> Integer
getDepartureDelay update = fromIntegral $ P'.getVal d delay
  where d = P'.getVal update STU.departure

-- private helpers
--
utf8ToString ::
  Utf8
  -> String
utf8ToString = U.toString . utf8
