{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
{- | Module to parse the real time feed for real time updates

This module uses protocol buffers to parse the feedmessage in order to update
the schedule data.

See also: https://developers.google.com/transit/gtfs-realtime/reference/
-}
module GTFS.Realtime.Message.Schedule
       (updateSchedule, updateSchedulesWithRealtimeData,
        FM.FeedMessage)
       where

import qualified Com.Google.Transit.Realtime.FeedMessage     as FM
import qualified Com.Google.Transit.Realtime.VehiclePosition as VP
import           GTFS.Realtime.Message.Types                 (ForFeedElement (..))
import           GTFS.Schedule                               (ScheduleItem (..))

import qualified Com.Google.Transit.Realtime.FeedEntity      as FE
import qualified Com.Google.Transit.Realtime.TripUpdate      as TU
import           Control.Monad.State                         (State, execState,
                                                              get, put)
import qualified Data.Map.Lazy                               as Map
import qualified Data.Text                                   as T
import qualified Text.ProtocolBuffers.Header                 as P'

import           Network.HTTP.Conduit                        (simpleHttp)
import           Text.ProtocolBuffers                        (messageGet)


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

getTripUpdates ::
  FM.FeedMessage
  -> P'.Seq TU.TripUpdate
getTripUpdates fm = (`P'.getVal` FE.trip_update) <$> entity
  where entity = P'.getVal fm FM.entity

getVehiclePositions ::
  FM.FeedMessage
  -> P'.Seq VP.VehiclePosition
getVehiclePositions fm = (`P'.getVal` FE.vehicle) <$> (P'.getVal fm FM.entity)

updateFeedElement ::
  ForFeedElement e => e
  -> State Schedule ()
updateFeedElement x = do
    m <- get
    let (_,map') =
            Map.updateLookupWithKey (updateScheduleItem x) (getTripID x) m
    put map'
