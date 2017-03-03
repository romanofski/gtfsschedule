{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fixtures where

import GTFS.Schedule
       (ScheduleItem(..), ScheduleState(..), Stop(..), VehicleInformation(..))
import GTFS.Realtime.Message.Types (departureTimeWithDelay)

import Data.Time.LocalTime (TimeOfDay(..))

import Control.Concurrent (forkIO, takeMVar, putMVar, newEmptyMVar, killThread)
import Data.Conduit.Network (runTCPServer, serverSettings, ServerSettings, AppData)
import Data.Streaming.Network (bindPortTCP, setAfterBind)
import Network.Socket (sClose)
import qualified Data.IORef as I
import Control.Exception.Lifted (IOException, try, onException, bracket)
import System.IO.Unsafe (unsafePerformIO)

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
