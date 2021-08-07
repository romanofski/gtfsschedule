{-# LANGUAGE OverloadedStrings #-}

{-
Copyright (C) - 2017-2021 Róman Joost <roman@bromeco.de>

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

-- | Module to parse the real time feed for real time updates
--
-- This module uses protocol buffers to parse the feedmessage in order to update
-- the schedule data.
--
-- See also: https://developers.google.com/transit/gtfs-realtime/reference/
module GTFS.Realtime.Message.Schedule
  ( updateSchedule,
    updateSchedulesWithRealtimeData,
    getTripUpdates,
    getVehiclePositions,
  )
where

import Control.Lens (toListOf) 
import Control.Monad.State (State, execState, get, put)
import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import Data.Traversable (mapM)
import GTFS.Realtime.Internal.Com.Google.Transit.Realtime.FeedEntity (trip_update, vehicle)
import GTFS.Realtime.Internal.Com.Google.Transit.Realtime.FeedMessage (FeedMessage, entity)
import GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripUpdate (TripUpdate)
import GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition (VehiclePosition)
import GTFS.Realtime.Message.Types (ForFeedElement (..))
import GTFS.Schedule (ScheduleItem (..))
import Network.HTTP.Conduit (simpleHttp)
import Text.ProtocolBuffers (messageGet)
import Prelude hiding (mapM)

type Schedule = Map.Map String ScheduleItem

-- | Updates the schedule with realtime information from the GTFS feed
updateSchedulesWithRealtimeData ::
  Maybe T.Text ->
  [ScheduleItem] ->
  IO [ScheduleItem]
updateSchedulesWithRealtimeData Nothing schedules = pure schedules
updateSchedulesWithRealtimeData (Just url) schedules = do
  bytes <- simpleHttp (T.unpack url)
  case messageGet bytes of
    Left err -> do
      print $ "Error occurred decoding feed: " ++ err
      pure schedules
    Right (fm, _) -> pure $ updateSchedule fm schedules

-- | Updates schedule with trip updates given by feed
updateSchedule ::
  FeedMessage ->
  [ScheduleItem] ->
  [ScheduleItem]
updateSchedule fm schedules =
  updateScheduleHelper getVehiclePositions fm $
    updateScheduleHelper getTripUpdates fm schedules

updateScheduleHelper ::
  ForFeedElement e =>
  (FeedMessage -> [e]) ->
  FeedMessage ->
  [ScheduleItem] ->
  [ScheduleItem]
updateScheduleHelper getter fm schedule =
  Map.elems $ execState (mapM updateFeedElement $ getter fm) scheduleMap
  where
    scheduleMap = Map.fromList $ toMap <$> schedule
    toMap x = (tripId x, x)

getTripUpdates ::
  FeedMessage ->
  [TripUpdate]
getTripUpdates = toListOf (entity . traverse . trip_update . traverse)

getVehiclePositions ::
  FeedMessage ->
  [VehiclePosition]
getVehiclePositions = toListOf (entity . traverse . vehicle . traverse)

updateFeedElement ::
  ForFeedElement e =>
  e ->
  State Schedule ()
updateFeedElement x = do
  m <- get
  let map' = Map.adjustWithKey (updateScheduleItem x) (getTripID x) m
  put map'
