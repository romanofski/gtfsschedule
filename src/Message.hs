-- | A real time update from the GTFS feed
module Message
    ( getDepartureRoute
    , getDepartureTime
    , parseFeedUpdate
    , formatUTCTime
    ) where

import Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate
import Com.Google.Transit.Realtime.TripUpdate.StopTimeEvent (StopTimeEvent(..))

import Text.ProtocolBuffers (messageGet, utf8)
import Text.ProtocolBuffers.Basic (Utf8)
import qualified Text.ProtocolBuffers.Header as P'
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.UTF8 as U (toString)
import Data.Time.LocalTime (LocalTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock (UTCTime)

-- | returns the feed update for a stop
-- TODO: return parse error?
--
parseFeedUpdate ::
  Lazy.ByteString
  -> Maybe StopTimeUpdate
parseFeedUpdate feed = case messageGet feed of
  Left _ -> Nothing
  Right (sched, _) -> Just sched

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
