{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fixtures where

import GTFS.Schedule
       (ScheduleItem(..), ScheduleState(..), Stop(..), VehicleInformation(..))
import GTFS.Realtime.Message.Types (departureTimeWithDelay)

import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripDescriptor as TD
import GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripUpdate (TripUpdate(..))
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate as STU
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate.ScheduleRelationship as STUSR
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripUpdate.StopTimeEvent as STE
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.TripDescriptor.ScheduleRelationship as TUSR
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition as VP
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition.CongestionLevel as VPCL
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.VehiclePosition.OccupancyStatus as VPOS
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.FeedEntity as FE
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.FeedHeader as FH
import qualified GTFS.Realtime.Internal.Com.Google.Transit.Realtime.FeedMessage as FM

import Data.Time.LocalTime (TimeOfDay(..), timeOfDayToTime)

import Text.ProtocolBuffers.Extensions (ExtField(..))
import Text.ProtocolBuffers.Basic (uFromString)
import Text.ProtocolBuffers.Unknown (UnknownField(..))

import Control.Concurrent (forkIO, takeMVar, putMVar, newEmptyMVar, killThread)
import Data.Conduit.Network (runTCPServer, serverSettings, ServerSettings, AppData)
import Data.Streaming.Network (bindPortTCP, setAfterBind)
import Network.Socket (sClose)
import qualified Data.IORef as I
import Control.Exception.Lifted (IOException, try, onException, bracket)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Map.Lazy as Map
import Data.Sequence (empty, Seq)

import Data.Functor ((<$>))

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
            sClose socket
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

testScheduleItem :: String -> TimeOfDay -> Integer -> Integer -> ScheduleItem
testScheduleItem sName depTime depDelay walktime = ScheduleItem
          { tripId = "."
          , stop = Stop
            { stopIdentifier = "."
            , stopWalktime = walktime
            , stopName = ""
            }
          , serviceName = sName
          , scheduledDepartureTime = depTime
          , departureDelay = depDelay
          , departureTime = departureTimeWithDelay depTime depDelay
          , scheduleType = SCHEDULED
          , scheduleItemVehicleInformation = VehicleInformation Nothing Nothing
          }

testExtField :: ExtField
testExtField = ExtField Map.empty

testUnknownField :: UnknownField
testUnknownField = UnknownField empty

testVehiclePosition
    :: Maybe String
    -> Maybe VPCL.CongestionLevel
    -> Maybe VPOS.OccupancyStatus
    -> Maybe String
    -> Maybe TUSR.ScheduleRelationship
    -> VP.VehiclePosition
testVehiclePosition sid cl os t sr =
    VP.VehiclePosition
    { VP.trip = Just $ testTripDescriptor t sr
    , VP.vehicle = Nothing
    , VP.position = Nothing
    , VP.current_stop_sequence = Nothing
    , VP.stop_id = uFromString <$> sid
    , VP.current_status = Nothing
    , VP.timestamp = Nothing
    , VP.congestion_level = cl
    , VP.occupancy_status = os
    , VP.ext'field = testExtField
    , VP.unknown'field = testUnknownField
    }

testTripDescriptor :: Maybe String -> Maybe TUSR.ScheduleRelationship -> TD.TripDescriptor
testTripDescriptor t sr =
    TD.TripDescriptor
    { TD.trip_id = uFromString <$> t
    , TD.route_id = Nothing
    , TD.direction_id = Nothing
    , TD.start_time = Nothing
    , TD.start_date = Nothing
    , TD.schedule_relationship = sr
    , TD.ext'field = testExtField
    , TD.unknown'field = testUnknownField
    }

testStopTimeEvent :: Maybe Integer -> Maybe TimeOfDay -> STE.StopTimeEvent
testStopTimeEvent d t =
    STE.StopTimeEvent
    { STE.delay = fromIntegral <$> d
    , STE.time = floor . toRational . timeOfDayToTime <$> t
    , STE.uncertainty = Nothing
    , STE.ext'field = testExtField
    , STE.unknown'field = testUnknownField
    }

testStopTimeUpdate
    :: Maybe String
    -> Maybe STE.StopTimeEvent
    -> Maybe STUSR.ScheduleRelationship
    -> STU.StopTimeUpdate
testStopTimeUpdate s d sr =
    STU.StopTimeUpdate
    { STU.stop_sequence = Just $ fromIntegral (0 :: Integer)
    , STU.stop_id = uFromString <$> s
    , STU.arrival = Nothing
    , STU.departure = d
    , STU.schedule_relationship = sr
    , STU.ext'field = testExtField
    , STU.unknown'field = testUnknownField
    }

testTripUpdate :: Maybe String -> Maybe TUSR.ScheduleRelationship -> Seq STU.StopTimeUpdate -> TripUpdate
testTripUpdate t sr stus =
    TripUpdate
    { trip = testTripDescriptor t sr
    , vehicle = Nothing
    , stop_time_update = stus
    , timestamp = Nothing
    , delay = Nothing
    , ext'field = testExtField
    , unknown'field = testUnknownField
    }

testFeedEntity :: Maybe TripUpdate -> Maybe VP.VehiclePosition -> FE.FeedEntity
testFeedEntity t vp =
    FE.FeedEntity
    { FE.id = uFromString "asdf"
    , FE.is_deleted = Nothing
    , FE.trip_update = t
    , FE.vehicle = vp
    , FE.alert = Nothing
    , FE.ext'field = testExtField
    , FE.unknown'field = testUnknownField
    }

testFeedHeader :: FH.FeedHeader
testFeedHeader =
    FH.FeedHeader
    { FH.gtfs_realtime_version = uFromString "1.1"
    , FH.incrementality = Nothing
    , FH.timestamp = Nothing
    , FH.ext'field = testExtField
    , FH.unknown'field = testUnknownField
    }

testFeedMessage :: Seq FE.FeedEntity -> FM.FeedMessage
testFeedMessage es =
    FM.FeedMessage
    { FM.header = testFeedHeader
    , FM.entity = es
    , FM.ext'field = testExtField
    , FM.unknown'field = testUnknownField
    }
