{-# LANGUAGE OverloadedStrings #-}
module Main where

import GTFS.Schedule
       (ScheduleItem(..), ScheduleState(..), TimeSpec(..),
        minutesToDeparture, formatScheduleItem, printSchedule,
        humanReadableDelay, getSchedule, sortSchedules, bumOffSeatTime)
import GTFS.Realtime.Message (departureTimeWithDelay)
import qualified GTFS.Database as DB
import qualified CSV.Import as CSV

import Realtime (feedTests)
import CSVImport (importTests)
import TestUpdate (updateTests)

import Control.Applicative ((<$>), (<*>))

import Data.List (sort)

import Test.Tasty (defaultMain, TestTree, TestName, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck
       (testProperty, elements, getPositive, choose, Arbitrary(..))

import Data.Time.LocalTime (TimeOfDay(..))
import Data.Time.Calendar (fromGregorian)
import Database.Persist (insert)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Logger (NoLoggingT(..))
import System.IO.Temp (withSystemTempFile)
import System.IO.Silently (capture)
import System.Directory (getCurrentDirectory)

import qualified Database.Persist.Sqlite as Sqlite

tests ::
  TestTree
tests = testGroup "tests" [proptests, unittests]

proptests :: TestTree
proptests = testGroup "property tests" [ testSortSchedules ]

instance Arbitrary TimeOfDay where
    arbitrary =
        TimeOfDay <$> choose (0, 23) <*> choose (0, 59) <*>
        (fromRational . toRational <$> choose (0 :: Double, 60))

instance Arbitrary ScheduleState where
  arbitrary = elements [CANCELED, ADDED, SCHEDULED]

instance Arbitrary ScheduleItem where
    arbitrary = do
        schedDepTime <- arbitrary
        delay <- arbitrary
        trip <- arbitrary
        stop <- arbitrary
        name <- arbitrary
        stype <- arbitrary
        return $ ScheduleItem { tripId = trip
                              , stopId = stop
                              , serviceName = name
                              , scheduledDepartureTime = schedDepTime
                              , departureDelay = delay
                              , departureTime = departureTimeWithDelay schedDepTime delay
                              , scheduleType = stype
                              }

testSortSchedules :: TestTree
testSortSchedules =
    testProperty
        "schedules are sorted by bum-off-seat-time"
        (\schedule ->
              propOrderedSchedule (sortSchedules schedule))

propOrderedSchedule :: [(Integer, ScheduleItem)] -> Bool
propOrderedSchedule [] = True
propOrderedSchedule [(x, i)] = True
propOrderedSchedule (x:y:rest) = (bumOffSeatTime x) <= (bumOffSeatTime y) && propOrderedSchedule rest

-- unit tests
--
unittests :: TestTree
unittests = testGroup "unit tests" [ feedTests
                               , importTests
                               , updateTests
                               , testMinutesToDeparture
                               , testFormatScheduleItem
                               , testDepartures
                               , testPrintSchedule
                               , testHumanReadableDelay
                               ]

testPrintSchedule ::
  TestTree
testPrintSchedule = testCase "prints empty schedule" $ do
  (output, _) <- capture $ printSchedule [] (TimeOfDay 7 7 7)
  output @?= "No services for the next 30min"

makeTest ::
  (Eq a, Show a)
  => (TestName, a, a)
  -> TestTree
makeTest (name, input, expected) = testCase name $ input @?= expected

testFormatScheduleItem ::
  TestTree
testFormatScheduleItem = testGroup "formates schedule item" $ makeTest <$>
  [ ("punctual", formatScheduleItem (TimeOfDay 7 45 0) 0 punctual, "Punctual 5min (07:50:00) ")
  , ("punctual with walking delay", formatScheduleItem (TimeOfDay 7 45 0) 2 punctual, "Punctual 3min (07:50:00) ")
  , ("running late", formatScheduleItem (TimeOfDay 7 45 0) 0 runningLate, "!Running Late 6min (07:51:00 (-04:40)) ")
  , ("running late + walking delay", formatScheduleItem (TimeOfDay 7 45 0) 2 runningLate, "!Running Late 4min (07:51:00 (-04:40)) ")
  , ("running ahead", formatScheduleItem (TimeOfDay 7 45 0) 0 runningAhead, "!Running Ahead 5min (07:50:00 (+04:20)) ")
  , ("running ahead + walk delay", formatScheduleItem (TimeOfDay 7 45 0) 2 runningAhead, "!Running Ahead 3min (07:50:00 (+04:20)) ")
  ]
    where
      punctual = ScheduleItem { tripId = "."
                              , stopId = "."
                              , serviceName = "Punctual"
                              , scheduledDepartureTime = TimeOfDay 7 50 00
                              , departureDelay = 0
                              , departureTime = TimeOfDay 7 50 00
                              , scheduleType = SCHEDULED
                              }
      runningAhead = ScheduleItem { tripId = "."
                                  , stopId = "."
                                  , serviceName = "Running Ahead"
                                  , scheduledDepartureTime = TimeOfDay 7 50 00
                                  , departureDelay = -260
                                  , departureTime = TimeOfDay 7 50 00  -- not consistent with delay
                                  , scheduleType = SCHEDULED
                                  }
      runningLate = ScheduleItem { tripId = "."
                                  , stopId = "."
                                  , serviceName = "Running Late"
                                  , scheduledDepartureTime = TimeOfDay 7 50 00
                                  , departureDelay = 280
                                  , departureTime = TimeOfDay 7 51 00
                                  , scheduleType = SCHEDULED
                                  }

testMinutesToDeparture ::
  TestTree
testMinutesToDeparture = testGroup "calculates right delay" $ map makeTest
  [ ("simple", minutesToDeparture item (TimeOfDay 7 45 00), 6)
  , ("departure in past", minutesToDeparture item (TimeOfDay 7 52 00), -1)
  ]
    where
      item = ScheduleItem { tripId = "7136402-BT2015-04_FUL-Weekday-00"
                          , stopId = "10795"
                          , serviceName = "Test Service"
                          , scheduledDepartureTime = TimeOfDay 7 50 00
                          , departureDelay = 60
                          , departureTime = TimeOfDay 7 51 00
                          , scheduleType = SCHEDULED
                          }


testHumanReadableDelay ::
  TestTree
testHumanReadableDelay =
    testGroup "shows user friendly delay" $
    map
        hrTest
        [ ( "seconds delayed"
          , (humanReadableDelay
                 ScheduleItem
                 { tripId = "_"
                 , stopId = "_"
                 , serviceName = "_"
                 , scheduledDepartureTime = TimeOfDay 0 0 0
                 , departureDelay = 40
                 , departureTime = TimeOfDay 0 0 0
                 , scheduleType = SCHEDULED
                 })
          , "-40s")
        , ( "minute late"
          , (humanReadableDelay
                 ScheduleItem
                 { tripId = "_"
                 , stopId = "_"
                 , serviceName = "_"
                 , scheduledDepartureTime = TimeOfDay 0 0 0
                 , departureDelay = 60
                 , departureTime = TimeOfDay 0 0 0
                 , scheduleType = SCHEDULED
                 })
          , "-01:00")
        , ( "minutes late"
          , (humanReadableDelay
                 ScheduleItem
                 { tripId = "_"
                 , stopId = "_"
                 , serviceName = "_"
                 , scheduledDepartureTime = TimeOfDay 0 0 0
                 , departureDelay = 455
                 , departureTime = TimeOfDay 0 0 0
                 , scheduleType = SCHEDULED
                 })
          , "-07:35")
        , ( "seconds ahead"
          , (humanReadableDelay
                 ScheduleItem
                 { tripId = "_"
                 , stopId = "_"
                 , serviceName = "_"
                 , scheduledDepartureTime = TimeOfDay 0 0 0
                 , departureDelay = -20
                 , departureTime = TimeOfDay 0 0 0
                 , scheduleType = SCHEDULED
                 })
          , "+20s")
        , ( "minutes ahead"
          , (humanReadableDelay
                 ScheduleItem
                 { tripId = "_"
                 , stopId = "_"
                 , serviceName = "_"
                 , scheduledDepartureTime = TimeOfDay 0 0 0
                 , departureDelay = -230
                 , departureTime = TimeOfDay 0 0 0
                 , scheduleType = SCHEDULED
                 })
          , "+03:50")]
  where
    hrTest (title,actual,expected) = testCase title (actual @?= expected)

makeDatabaseImportTest ::
  TestInput
  -> TestTree
makeDatabaseImportTest (TestInput name csvdatadir scode timespec expected) =
  testCase name $
  do withSystemTempFile
       "GTFSTest"
       (\tmpfile _ ->
          do
             cwd <- getCurrentDirectory
             CSV.runImport tmpfile $ concat [cwd, "/", "test", "/", "data", "/", csvdatadir]
             schedule <- getSchedule tmpfile scode timespec
             schedule @?= expected)


data TestInput = TestInput { testName :: String
                           , csvdatadirectory :: String
                           , stopcode :: String
                           , now :: TimeSpec
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
      , stopcode = "600029"
      , now = TimeSpec (TimeOfDay 8 5 0) (fromGregorian 2015 2 7)
      , testExpectedSchedule = []
      }
    , TestInput
      { testName = "no departure because time is past all scheduled services"
      , csvdatadirectory = "regular"
      , stopcode = "600029"
      , now = TimeSpec (TimeOfDay 8 5 0) (fromGregorian 2013 1 7)
      , testExpectedSchedule = []
      }
    , TestInput
      { testName = "imports aftermidnight services"
      , csvdatadirectory = "aftermidnight"
      , stopcode = "600029"
      , now = TimeSpec (TimeOfDay 1 0 0) (fromGregorian 2013 2 4)
      , testExpectedSchedule = [ ScheduleItem
                                 { tripId = "1"
                                 , stopId = "600029"
                                 , serviceName = "66 Graveyard Express"
                                 , scheduledDepartureTime = TimeOfDay 1 1 0
                                 , departureDelay = 0
                                 , departureTime = TimeOfDay 1 1 0
                                 , scheduleType = SCHEDULED
                                 }]
      }
    , TestInput
      { testName = "additional temp scheduled service"
      , csvdatadirectory = "tempservice"
      , stopcode = "600029"
      , now = TimeSpec (TimeOfDay 8 5 0) (fromGregorian 2015 1 28)
      , testExpectedSchedule = [ ScheduleItem
                                 { tripId = "QF0815-00"
                                 , stopId = "600029"
                                 , serviceName = "66 not relevant"
                                 , scheduledDepartureTime = TimeOfDay 8 5 0
                                 , departureDelay = 0
                                 , departureTime = TimeOfDay 8 5 0
                                 , scheduleType = SCHEDULED
                                 }
                               , ScheduleItem
                                 { tripId = "QF0815-00-Ekka"
                                 , stopId = "600029"
                                 , serviceName = "66 not relevant"
                                 , scheduledDepartureTime = TimeOfDay 8 5 33
                                 , departureDelay = 0
                                 , departureTime = TimeOfDay 8 5 33
                                 , scheduleType = SCHEDULED
                                 }
                               , ScheduleItem
                                 { tripId = "QF0815-00"
                                 , stopId = "600029"
                                 , serviceName = "66 not relevant"
                                 , scheduledDepartureTime = TimeOfDay 8 21 33
                                 , departureDelay = 0
                                 , departureTime = TimeOfDay 8 21 33
                                 , scheduleType = SCHEDULED
                                 }]
      }
    , TestInput
      { testName = "includes only temp scheduled service"
      , csvdatadirectory = "tempservice"
      , stopcode = "600029"
      , now = TimeSpec (TimeOfDay 8 5 0) (fromGregorian 2015 2 4)
      , testExpectedSchedule = [ ScheduleItem
                                 { tripId = "QF0815-00-Ekka"
                                 , stopId = "600029"
                                 , serviceName = "66 not relevant"
                                 , scheduledDepartureTime = TimeOfDay 8 5 33
                                 , departureDelay = 0
                                 , departureTime = TimeOfDay 8 5 33
                                 , scheduleType = SCHEDULED
                                 }]
      }]

main ::
  IO ()
main = defaultMain tests
