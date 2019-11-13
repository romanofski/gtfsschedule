{-# LANGUAGE OverloadedStrings #-}
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
module TestSchedule (scheduleTests) where

import           Fixtures                 (cleanUpIfExist,
                                           generateTestUserDBFilePath,
                                           testScheduleItem)

import qualified CSV.Import               as CSV
import           GTFS.Schedule            (ScheduleConfig (..),
                                           ScheduleItem (..),
                                           ScheduleState (..), Stop (..),
                                           TimeSpec (..),
                                           VehicleInformation (..),
                                           defaultScheduleConfig, getSchedule,
                                           humanReadableDelay,
                                           minutesToDeparture, printSchedule)

import           Control.Applicative      ((<$>))

import           Control.Exception.Lifted (bracket)
import           Test.Tasty               (TestName, TestTree, testGroup)
import           Test.Tasty.HUnit         (testCase, (@?=))

import           Data.Time.Calendar       (fromGregorian)
import           Data.Time.LocalTime      (TimeOfDay (..))
import           System.Directory         (getCurrentDirectory)
import           System.IO.Silently       (hCapture_)
import           System.IO                (stderr)


scheduleTests ::
  TestTree
scheduleTests =
    testGroup
        "realtime_feed_tests"
        [ testMinutesToDeparture
        , testFormatScheduleItem
        , testDepartures
        , testPrintSchedule
        , testHumanReadableDelay]

testPrintSchedule ::
  TestTree
testPrintSchedule = testCase "prints empty schedule" $ do
  output <- hCapture_ [stderr] $ printSchedule [] (defaultScheduleConfig $ TimeOfDay 7 7 7) stderr
  output @?= "No services for the next 30min"

makeTest ::
  (TestName, [ScheduleItem], ScheduleConfig, String)
  -> TestTree
makeTest (name, input, cfg, expected) = testCase name $ do
  output <- hCapture_ [stderr] $ printSchedule input cfg stderr
  output @?= expected

testFormatScheduleItem ::
  TestTree
testFormatScheduleItem =
    testGroup "formats schedule item" $
    makeTest <$>
    [ ( "punctual"
      , [punctual]
      , (defaultScheduleConfig $ TimeOfDay 7 45 0)
      , "Punctual 5min 07:50:00  ")
    , ( "punctual with walking delay"
      , [ testScheduleItem "Punctual" (TimeOfDay 7 50 0) 0 2]
      , (defaultScheduleConfig $ TimeOfDay 7 45 0)
      , "Punctual 3min 07:50:00  ")
    , ( "running late"
      , [runningLate]
      , (defaultScheduleConfig $ TimeOfDay 7 45 0)
      , "!Running Late 10min 07:54:40 +04:40 ")
    , ( "running late + walking delay"
      , [ testScheduleItem "Running Late" (TimeOfDay 7 50 0) 280 2]
      , (defaultScheduleConfig $ TimeOfDay 7 45 0)
      , "!Running Late 8min 07:54:40 +04:40 ")
    , ( "running ahead"
      , [runningAhead]
      , (defaultScheduleConfig $ TimeOfDay 7 45 0)
      , "!Running Ahead 6min 07:50:40 -04:20 ")
    , ( "running ahead + walk delay"
      , [ testScheduleItem "Running Ahead" (TimeOfDay 7 55 0) (-260) 2]
      , (defaultScheduleConfig $ TimeOfDay 7 45 0)
      , "!Running Ahead 4min 07:50:40 -04:20 ")
    , ( "custom template"
      , [runningAhead]
      , (ScheduleConfig (TimeOfDay 7 45 0) "$serviceName$")
      , "Running Ahead")]
  where
    punctual = testScheduleItem "Punctual" (TimeOfDay 7 50 0) 0 0
    runningAhead = testScheduleItem "Running Ahead" (TimeOfDay 7 55 0) (-260) 0
    runningLate = testScheduleItem "Running Late" (TimeOfDay 7 50 0) 280 0

testMinutesToDeparture ::
  TestTree
testMinutesToDeparture =
    testGroup "calculates right delay" $
    (\(n,minutes,expected) ->
          testCase n (minutes @?= expected)) <$>
    [ ("simple", minutesToDeparture item (TimeOfDay 7 45 0), 6)
    , ("departure in past", minutesToDeparture item (TimeOfDay 7 52 0), -1)]
  where
    item = testScheduleItem "Test Service" (TimeOfDay 7 50 0) 60 0

testHumanReadableDelay ::
  TestTree
testHumanReadableDelay =
    testGroup "shows user friendly delay" $
    map
        hrTest
        [ ( "seconds delayed"
          , (humanReadableDelay $ testScheduleItem "_" (TimeOfDay 0 0 0) 40 0)
          , Just "+40s")
        , ( "minute late"
          , (humanReadableDelay $ testScheduleItem "_" (TimeOfDay 0 0 0) 60 0)
          , Just "+01:00")
        , ( "minutes late"
          , (humanReadableDelay $ testScheduleItem "_" (TimeOfDay 0 0 0) 455 0)
          , Just "+07:35")
        , ( "seconds ahead"
          , (humanReadableDelay $
             testScheduleItem "_" (TimeOfDay 0 0 0) (-20) 0)
          , Just "-20s")
        , ( "minutes ahead"
          , (humanReadableDelay $
             testScheduleItem "_" (TimeOfDay 0 0 0) (-230) 0)
          , Just "-03:50")]
  where
    hrTest (title,actual,expected) = testCase title (actual @?= expected)

makeDatabaseImportTest ::
  TestInput
  -> TestTree
makeDatabaseImportTest (TestInput name csvdatadir scode timespec expected) =
    testCase name $
    bracket
        (do generateTestUserDBFilePath "departureTestWithImports")
        cleanUpIfExist
        (\dbfile ->
              do cwd <- getCurrentDirectory
                 CSV.runImport dbfile $
                     concat [cwd, "/", "test", "/", "data", "/", csvdatadir]
                 schedule <- getSchedule dbfile scode timespec 3
                 schedule @?= expected)


data TestInput = TestInput { testName             :: String
                           , csvdatadirectory     :: String
                           , stop'                :: Stop
                           , now                  :: TimeSpec
                           , testExpectedSchedule :: [ScheduleItem]
                           }

testDepartures ::
  TestTree
testDepartures =
    testGroup "departure tests with imports" $
    makeDatabaseImportTest <$>
    [ TestInput
      { testName = "no departure because date is past all scheduled services"
      , csvdatadirectory = "regular"
      , stop' = Stop
        { stopIdentifier = "600029"
        , stopWalktime = 0
        , stopName = ""
        }
      , now = TimeSpec (TimeOfDay 8 5 0) (fromGregorian 2015 2 7)
      , testExpectedSchedule = []
      }
    , TestInput
      { testName = "no departure because service ends at stop"
      , csvdatadirectory = "servicefinalstop"
      , stop' = Stop
        { stopIdentifier = "600019"
        , stopWalktime = 0
        , stopName = ""
        }
      , now = TimeSpec (TimeOfDay 7 57 0) (fromGregorian 2015 1 1)
      , testExpectedSchedule = [ ScheduleItem
                                 { tripId = "QF0816-22"
                                 , stop = Stop
                                   { stopIdentifier = "600019"
                                   , stopWalktime = 0
                                   , stopName = "not relevant"
                                   }
                                 , serviceName = "42 not relevant"
                                 , scheduledDepartureTime = TimeOfDay 8 6 22
                                 , departureDelay = 0
                                 , departureTime = TimeOfDay 8 6 22
                                 , scheduleType = SCHEDULED
                                 , scheduleItemVehicleInformation = VehicleInformation
                                       Nothing
                                       Nothing
                                 }]
      }
    , TestInput
      { testName = "no departure because time is past all scheduled services"
      , csvdatadirectory = "regular"
      , stop' = Stop
        { stopIdentifier = "600029"
        , stopWalktime = 0
        , stopName = ""
        }
      , now = TimeSpec (TimeOfDay 8 5 0) (fromGregorian 2013 1 7)
      , testExpectedSchedule = []
      }
    , TestInput
      { testName = "imports aftermidnight services"
      , csvdatadirectory = "aftermidnight"
      , stop' = Stop
        { stopIdentifier = "600029"
        , stopWalktime = 0
        , stopName = ""
        }
      , now = TimeSpec (TimeOfDay 1 0 0) (fromGregorian 2013 2 4)
      , testExpectedSchedule = [ ScheduleItem
                                 { tripId = "1"
                                 , stop = Stop
                                   { stopIdentifier = "600029"
                                   , stopWalktime = 0
                                   , stopName = "not relevant"
                                   }
                                 , serviceName = "66 Graveyard Express"
                                 , scheduledDepartureTime = TimeOfDay 1 1 0
                                 , departureDelay = 0
                                 , departureTime = TimeOfDay 1 1 0
                                 , scheduleType = SCHEDULED
                                 , scheduleItemVehicleInformation = VehicleInformation
                                       Nothing
                                       Nothing
                                 }
                               , ScheduleItem
                                 { tripId = "2"
                                 , stop = Stop
                                   { stopIdentifier = "600029"
                                   , stopWalktime = 0
                                   , stopName = "not relevant"
                                   }
                                 , serviceName = "67 Hell Express"
                                 , scheduledDepartureTime = TimeOfDay 2 1 0
                                 , departureDelay = 0
                                 , departureTime = TimeOfDay 2 1 0
                                 , scheduleType = SCHEDULED
                                 , scheduleItemVehicleInformation = VehicleInformation
                                       Nothing
                                       Nothing
                                 }]
      }
    , TestInput
      { testName = "additional temp scheduled service"
      , csvdatadirectory = "tempservice"
      , stop' = Stop
        { stopIdentifier = "600029"
        , stopWalktime = 0
        , stopName = ""
        }
      , now = TimeSpec (TimeOfDay 8 5 0) (fromGregorian 2015 1 28)
      , testExpectedSchedule = [ ScheduleItem
                                 { tripId = "QF0815-00-Ekka"
                                 , stop = Stop
                                   { stopIdentifier = "600029"
                                   , stopWalktime = 0
                                   , stopName = "not relevant"
                                   }
                                 , serviceName = "66 not relevant"
                                 , scheduledDepartureTime = TimeOfDay 8 5 33
                                 , departureDelay = 0
                                 , departureTime = TimeOfDay 8 5 33
                                 , scheduleType = SCHEDULED
                                 , scheduleItemVehicleInformation = VehicleInformation
                                       Nothing
                                       Nothing
                                 }
                               , ScheduleItem
                                 { tripId = "QF0815-00"
                                 , stop = Stop
                                   { stopIdentifier = "600029"
                                   , stopWalktime = 0
                                   , stopName = "not relevant"
                                   }
                                 , serviceName = "66 not relevant"
                                 , scheduledDepartureTime = TimeOfDay 8 8 0
                                 , departureDelay = 0
                                 , departureTime = TimeOfDay 8 8 0
                                 , scheduleType = SCHEDULED
                                 , scheduleItemVehicleInformation = VehicleInformation
                                       Nothing
                                       Nothing
                                 }]
      }
    , TestInput
      { testName = "includes only temp scheduled service"
      , csvdatadirectory = "tempservice"
      , stop' = Stop
        { stopIdentifier = "600029"
        , stopWalktime = 0
        , stopName = ""
        }
      , now = TimeSpec (TimeOfDay 8 5 0) (fromGregorian 2015 2 4)
      , testExpectedSchedule = [ ScheduleItem
                                 { tripId = "QF0815-00-Ekka"
                                 , stop = Stop
                                   { stopIdentifier = "600029"
                                   , stopWalktime = 0
                                   , stopName = "not relevant"
                                   }
                                 , serviceName = "66 not relevant"
                                 , scheduledDepartureTime = TimeOfDay 8 5 33
                                 , departureDelay = 0
                                 , departureTime = TimeOfDay 8 5 33
                                 , scheduleType = SCHEDULED
                                 , scheduleItemVehicleInformation = VehicleInformation
                                       Nothing
                                       Nothing
                                 }]
      }]
