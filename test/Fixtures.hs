{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

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
module Fixtures where

import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Exception.Lifted (IOException, bracket, onException, try)
import Data.Conduit.Network (AppData, ServerSettings, runTCPServer, serverSettings)
import qualified Data.IORef as I
import qualified Data.Map.Lazy as Map
import Data.Sequence (Seq, empty)
import Data.Streaming.Network (bindPortTCP, setAfterBind)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.LocalTime (TimeOfDay (..), timeOfDayToTime)
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.FeedEntity as FE
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.FeedHeader as FH
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.FeedMessage as FM
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripDescriptor as TD
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripDescriptor.ScheduleRelationship as TUSR
import GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripUpdate (TripUpdate (..))
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripUpdate.StopTimeEvent as STE
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate as STU
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate.ScheduleRelationship as STUSR
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition as VP
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition.CongestionLevel as VPCL
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition.OccupancyStatus as VPOS
import GTFS.Realtime.Message.Types (departureTimeWithDelay)
import GTFS.Schedule (ScheduleItem (..), ScheduleState (..), Stop (..), VehicleInformation (..))
import Network.Socket (close)
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removeFile)
import System.IO.Unsafe (unsafePerformIO)
import Text.ProtocolBuffers.Basic (uFromString)
import Text.ProtocolBuffers.Extensions (ExtField (..))
import Text.ProtocolBuffers.Unknown (UnknownField (..))

serverHost :: String
serverHost = "127.0.0.1"

nextPort :: I.IORef Int
nextPort = unsafePerformIO $ I.newIORef 1542
{-# NOINLINE nextPort #-}

getPort :: IO Int
getPort = do
  port <-
    I.atomicModifyIORef nextPort $
      \p ->
        (p + 1, p + 1)
  esocket <- try $ bindPortTCP port "*4"
  case esocket of
    Left (_ :: IOException) -> getPort
    Right socket -> do
      close socket
      return port

--
-- taken from https://github.com/snoyberg/http-client/blob/master/http-conduit/test/main.hs (withCApp)
--
withConcurrentTCPServer :: (AppData -> IO ()) -> (Int -> IO ()) -> IO ()
withConcurrentTCPServer app f = do
  port <- getPort
  baton <- newEmptyMVar
  let start = do
        putMVar baton ()
      settings :: ServerSettings
      settings = setAfterBind (const start) (serverSettings port "127.0.0.1")
  bracket
    (forkIO $ runTCPServer settings app `onException` start)
    killThread
    (const $ takeMVar baton >> f port)

generateTestUserDBFilePath :: String -> IO FilePath
generateTestUserDBFilePath template = do
  tmpdir <- getTemporaryDirectory
  dir <- generateName template
  dbfile <- generateName template
  let dirtree = concatMap (\x -> concat ["/", x]) [tmpdir, dir]
  let newdbfile = dirtree ++ "/" ++ dbfile
  createDirectoryIfMissing True dirtree
  return newdbfile

generateName :: String -> IO String
generateName template = do
  time <- round <$> getPOSIXTime
  return $ template ++ (show time)

cleanUpIfExist :: FilePath -> IO ()
cleanUpIfExist fp = do
  fpExists <- doesFileExist fp
  if fpExists
    then removeFile fp
    else return ()

testScheduleItem :: String -> TimeOfDay -> Integer -> Integer -> ScheduleItem
testScheduleItem sName depTime depDelay walktime =
  ScheduleItem
    { tripId = ".",
      stop =
        Stop
          { stopIdentifier = ".",
            stopWalktime = walktime,
            stopName = ""
          },
      serviceName = sName,
      scheduledDepartureTime = depTime,
      departureDelay = depDelay,
      departureTime = departureTimeWithDelay depTime depDelay,
      scheduleType = SCHEDULED,
      scheduleItemVehicleInformation = VehicleInformation Nothing Nothing
    }

testExtField :: ExtField
testExtField = ExtField Map.empty

testUnknownField :: UnknownField
testUnknownField = UnknownField empty

testVehiclePosition ::
  Maybe String ->
  Maybe VPCL.CongestionLevel ->
  Maybe VPOS.OccupancyStatus ->
  Maybe String ->
  Maybe TUSR.ScheduleRelationship ->
  VP.VehiclePosition
testVehiclePosition sid cl os t sr =
  VP.VehiclePosition
    { VP._trip = Just $ testTripDescriptor t sr,
      VP._vehicle = Nothing,
      VP._position = Nothing,
      VP._current_stop_sequence = Nothing,
      VP._stop_id = uFromString <$> sid,
      VP._current_status = Nothing,
      VP._timestamp = Nothing,
      VP._congestion_level = cl,
      VP._occupancy_status = os,
      VP._ext'field = testExtField,
      VP._unknown'field = testUnknownField
    }

testTripDescriptor :: Maybe String -> Maybe TUSR.ScheduleRelationship -> TD.TripDescriptor
testTripDescriptor t sr =
  TD.TripDescriptor
    { TD._trip_id = uFromString <$> t,
      TD._route_id = Nothing,
      TD._direction_id = Nothing,
      TD._start_time = Nothing,
      TD._start_date = Nothing,
      TD._schedule_relationship = sr,
      TD._ext'field = testExtField,
      TD._unknown'field = testUnknownField
    }

testStopTimeEvent :: Maybe Integer -> Maybe TimeOfDay -> STE.StopTimeEvent
testStopTimeEvent d t =
  STE.StopTimeEvent
    { STE._delay = fromIntegral <$> d,
      STE._time = floor . toRational . timeOfDayToTime <$> t,
      STE._uncertainty = Nothing,
      STE._ext'field = testExtField,
      STE._unknown'field = testUnknownField
    }

testStopTimeUpdate ::
  Maybe String ->
  Maybe STE.StopTimeEvent ->
  Maybe STUSR.ScheduleRelationship ->
  STU.StopTimeUpdate
testStopTimeUpdate s d sr =
  STU.StopTimeUpdate
    { STU._stop_sequence = Just $ fromIntegral (0 :: Integer),
      STU._stop_id = uFromString <$> s,
      STU._arrival = Nothing,
      STU._departure = d,
      STU._schedule_relationship = sr,
      STU._ext'field = testExtField,
      STU._unknown'field = testUnknownField
    }

testTripUpdate :: Maybe String -> Maybe TUSR.ScheduleRelationship -> Seq STU.StopTimeUpdate -> TripUpdate
testTripUpdate t sr stus =
  TripUpdate
    { _trip = testTripDescriptor t sr,
      _vehicle = Nothing,
      _stop_time_update = stus,
      _timestamp = Nothing,
      _delay = Nothing,
      _ext'field = testExtField,
      _unknown'field = testUnknownField
    }

testFeedEntity :: Maybe TripUpdate -> Maybe VP.VehiclePosition -> FE.FeedEntity
testFeedEntity t vp =
  FE.FeedEntity
    { FE._id = uFromString "asdf",
      FE._is_deleted = Nothing,
      FE._trip_update = t,
      FE._vehicle = vp,
      FE._alert = Nothing,
      FE._ext'field = testExtField,
      FE._unknown'field = testUnknownField
    }

testFeedHeader :: FH.FeedHeader
testFeedHeader =
  FH.FeedHeader
    { FH._gtfs_realtime_version = uFromString "1.1",
      FH._incrementality = Nothing,
      FH._timestamp = Nothing,
      FH._ext'field = testExtField,
      FH._unknown'field = testUnknownField
    }

testFeedMessage :: Seq FE.FeedEntity -> FM.FeedMessage
testFeedMessage es =
  FM.FeedMessage
    { FM._header = testFeedHeader,
      FM._entity = es,
      FM._ext'field = testExtField,
      FM._unknown'field = testUnknownField
    }
