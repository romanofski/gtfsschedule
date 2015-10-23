module Lib
    ( showMessage
    ) where

import qualified Control.Exception as CE
import Network.HTTP
import Network.URI (parseURI)
import Text.ProtocolBuffers (messageGet)
import qualified Data.ByteString.Lazy as Lazy
import Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate
import Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate.ScheduleRelationship

-- | TODO: Currently hardcoded SEQ translink GTFS feed
--
translinkFeedURI :: String
translinkFeedURI = "http://gtfsrt.api.translink.com.au/Feed/SEQ"

downloadFeed :: IO StopTimeUpdate
downloadFeed = do
    let request = getLazyRequest
    resp <- simpleHTTP request >>= getResponseBody
    case messageGet (resp) of
      Left msg -> error ("Failed to parse stream\n" ++ msg)
      Right (fmsg, _) -> return fmsg

getLazyRequest :: Request Lazy.ByteString
getLazyRequest = case parseURI translinkFeedURI of
  Nothing -> error "Error in URL"
  Just url -> mkRequest GET url


showMessage :: IO ()
showMessage = do
  f <- downloadFeed
  putStrLn $ show f
