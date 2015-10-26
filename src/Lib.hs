module Lib
    ( showMessage
    , getSchedule
    ) where

import qualified Control.Exception as CE
import Network.HTTP
import Network.URI (parseURI)
import qualified Data.ByteString.Lazy as Lazy

-- | TODO: Currently hardcoded SEQ translink GTFS feed
--
translinkFeedURI :: String
translinkFeedURI = "http://gtfsrt.api.translink.com.au/Feed/SEQ"

downloadFeed :: IO Lazy.ByteString
downloadFeed = do
    let request = getLazyRequest
    resp <- simpleHTTP request >>= getResponseBody
    resp
    -- empty :(
    -- Lazy.writeFile "/tmp/gtfs-feed" resp
    -- case messageGet (resp) of
      -- Left msg -> error ("Failed to parse stream\n" ++ msg)
      -- Right (fmsg, _) -> return fmsg

getLazyRequest :: Request Lazy.ByteString
getLazyRequest = case parseURI translinkFeedURI of
  Nothing -> error "Error in URL"
  Just url -> mkRequest GET url


showMessage :: IO ()
showMessage = do
  f <- downloadFeed
  putStrLn $ show f

-- | Data type indicating a leaving vehicle
--
-- TODO: need route and route, direction
--
data Departure = Departure String UTCTime
               deriving Show

