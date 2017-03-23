{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-
Copyright (C) - 2017 Róman Joost <roman@bromeco.de>

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
module TestCSVImport (importTests) where

import qualified CSV.Import               as CSV
import qualified GTFS.Database            as DB
import           GTFS.Schedule            (ScheduleItem (..),
                                           ScheduleState (..), Stop (..),
                                           TimeSpec (..),
                                           VehicleInformation (..), getSchedule)

import           Fixtures                 (serverHost, withConcurrentTCPServer)

import           Data.Functor             ((<$>))
import           Data.Time.Calendar       (fromGregorian)
import           Data.Time.Clock          (UTCTime (..), getCurrentTime)
import           Data.Time.Clock.POSIX    (getPOSIXTime)
import           Data.Time.LocalTime      (TimeOfDay (..))
import           Test.Tasty               (TestTree, testGroup)
import           Test.Tasty.HUnit         (testCase, (@?=))

import           Control.Exception.Lifted (onException)
import           Control.Monad.IO.Class   (liftIO)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as C8 (pack)
import           Data.Conduit             (yield, ($$))
import           Data.Conduit.Network     (AppData, appSink)
import           System.Directory         (doesFileExist, getTemporaryDirectory,
                                           removeFile)

import qualified Data.Text                as T
import           System.IO.Temp           (withSystemTempFile)


importTests ::
  TestTree
importTests = testGroup "import tests"
            [ testImportWithExistingDBFile
            , testImportWithoutExistingDBFile
            ]

testImportWithExistingDBFile :: TestTree
testImportWithExistingDBFile =
    testCase "imports all successfully" $
    withConcurrentTCPServer withHTTPAppData $
    \port ->
         do withSystemTempFile
                "ImportTest"
                (\tmpfile _ ->
                      do let url =
                                 concat ["http://", serverHost, ":", show port]
                         CSV.createNewDatabase url tmpfile
                         schedule <-
                             getSchedule
                                 tmpfile
                                 Stop
                                 { stopIdentifier = "600029"
                                 , stopWalktime = 7
                                 , stopName = ""
                                 }
                                 (TimeSpec
                                      (TimeOfDay 8 5 0)
                                      (fromGregorian 2015 1 28))
                                 3
                         schedule @?=
                             [ ScheduleItem
                               { tripId = "QF0815-00"
                               , stop = Stop
                                 { stopIdentifier = "600029"
                                 , stopWalktime = 7
                                 , stopName = "not relevant"
                                 }
                               , serviceName = "66 not relevant"
                               , scheduledDepartureTime = (TimeOfDay 8 5 0)
                               , departureDelay = 0
                               , departureTime = (TimeOfDay 8 5 0)
                               , scheduleType = SCHEDULED
                               , scheduleItemVehicleInformation = VehicleInformation Nothing Nothing
                               }
                             , ScheduleItem
                               { tripId = "QF0815-00"
                               , stop = Stop
                                 { stopIdentifier = "600029"
                                 , stopWalktime = 7
                                 , stopName = "not relevant"
                                 }
                               , serviceName = "66 not relevant"
                               , scheduledDepartureTime = (TimeOfDay 8 21 33)
                               , departureDelay = 0
                               , departureTime = (TimeOfDay 8 21 33)
                               , scheduleType = SCHEDULED
                               , scheduleItemVehicleInformation = VehicleInformation Nothing Nothing
                               }])


testImportWithoutExistingDBFile :: TestTree
testImportWithoutExistingDBFile =
    testCase "imports by creating DB file" $
    withConcurrentTCPServer withHTTPAppData $
    \port ->
         do newdbfile <- generateTestUserDBFilePath
            onException (runImport port newdbfile) (cleanUpIfExist newdbfile)
            now <- getCurrentTime
            day <- DB.getLastUpdatedDatabase (T.pack newdbfile)
            day @?= utctDay now
  where
    runImport p userdbfile = do
        let url = concat ["http://", serverHost, ":", show p]
        CSV.createNewDatabase url userdbfile


generateTestUserDBFilePath :: IO FilePath
generateTestUserDBFilePath = do
  tmpdir <- getTemporaryDirectory
  dir <- generateName
  dbfile <- generateName
  let dirtree = concatMap (\x -> concat ["/", x]) [dir, dbfile]
  let newdbfile = tmpdir ++ dirtree
  return newdbfile

generateName :: IO String
generateName = do
  time <- round <$> getPOSIXTime
  return $ template ++ (show time)
    where template = "importest"

cleanUpIfExist :: FilePath -> IO ()
cleanUpIfExist fp = do
    fpExists <- doesFileExist fp
    if fpExists
        then removeFile fp
        else return ()

withHTTPAppData :: AppData -> IO ()
withHTTPAppData appData = src $$ appSink appData
  where
    src = do
      yield "HTTP/1.1 200 OK\r\nContent-Type: application/x-zip-compressed\r\n"
      contents <- liftIO $ BS.readFile $ concat ["test", "/", "data", "/", "regular.zip"]
      let clength = BS.concat ["Content-Length: ", (C8.pack $ show $ BS.length contents), "\r\n"]
      yield $ BS.concat [clength, "\r\n", contents]
