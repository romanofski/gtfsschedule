{-# LANGUAGE OverloadedStrings #-}
-- | A real time update from the GTFS feed
module Message where

import Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate
import Com.Google.Transit.Realtime.TripUpdate.StopTimeEvent (StopTimeEvent(..))
import Com.Google.Transit.Realtime.TripDescriptor (trip_id)
import qualified Com.Google.Transit.Realtime.FeedMessage as FM
import qualified Com.Google.Transit.Realtime.TripUpdate as TU
import qualified Com.Google.Transit.Realtime.FeedEntity as FE

import qualified Database.Persist.Sqlite as Sqlite

import Text.ProtocolBuffers (messageGet, utf8)
import Text.ProtocolBuffers.Basic (Utf8)
import qualified Text.ProtocolBuffers.Header as P'
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import Data.Time.LocalTime (LocalTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock (UTCTime)
import Control.Monad ( mfilter
                     , join)

import Database ( Trip
                , tripTripId
                )

-- TODO unsafe!
--
parseFeedUpdate ::
  L.ByteString
  -> FM.FeedMessage
parseFeedUpdate feed = case messageGet feed of
  Left _ -> error "Shit happened"
  Right (fm, _) -> fm

getFeedEntities ::
  FM.FeedMessage
  -> P'.Seq TU.TripUpdate
getFeedEntities fm = (`P'.getVal` FE.trip_update) <$> entity
  where entity = P'.getVal fm FM.entity

filterTripUpdate ::
  [Sqlite.Entity Trip]
  -> P'.Seq TU.TripUpdate
  -> P'.Seq TU.TripUpdate
filterTripUpdate xs = mfilter (\x -> getTripID x `elem` relevantTripIDs)
  where relevantTripIDs = tripTripId . Sqlite.entityVal <$> xs

getTripID ::
  TU.TripUpdate
  -> String
getTripID x = utf8ToString tripId
  where
    descriptor = P'.getVal x TU.trip
    tripId = P'.getVal descriptor trip_id

-- | Returns a nice departure route
-- TODO
getDepartureRoute ::
  StopTimeUpdate
  -> String
getDepartureRoute msg = utf8ToString route
  where route = P'.getVal msg stop_id

getDepartureTime ::
  StopTimeEvent
  -> UTCTime
getDepartureTime event = depTime
  where d = toInteger $ P'.getVal event delay
        t = toInteger $ P'.getVal event time
        depTime = secondsToTime $ d + t

-- | format (e.g. departure) time to use local time
-- Note: Uses defaultTimeLocale
--
formatUTCTime ::
  LocalTime
  -> String
formatUTCTime = formatTime defaultTimeLocale "%R"


-- private helpers
--
utf8ToString ::
  Utf8
  -> String
utf8ToString = U.toString . utf8

-- | convert our unix epoch time to a real time format so we can use it to display departure time
--
secondsToTime ::
  Integral a =>
  a
  -> UTCTime
secondsToTime x = posixSecondsToUTCTime $ realToFrac x
