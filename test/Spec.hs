{-# LANGUAGE OverloadedStrings #-}
module Main where

import GTFS.Schedule
       (ScheduleItem(..), ScheduleState(..), TimeSpec(..),
        ScheduleConfig(..), Stop(..), minutesToDeparture, printSchedule,
        humanReadableDelay, getSchedule, sortSchedules, bumOffSeatTime,
        defaultScheduleConfig)
import GTFS.Realtime.Message (departureTimeWithDelay)
import qualified CSV.Import as CSV

import Realtime (feedTests)
import CSVImport (importTests)
import TestUpdate (updateTests)

import Control.Applicative ((<$>), (<*>))

import Test.Tasty (defaultMain, TestTree, TestName, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck
       (testProperty, elements, choose, Arbitrary(..), Gen, Positive(..))

import Data.Time.LocalTime (TimeOfDay(..))
import Data.Time.Calendar (fromGregorian)
import System.IO.Temp (withSystemTempFile)
import System.IO.Silently (capture_)
import System.Directory (getCurrentDirectory)


tests ::
  TestTree
tests = testGroup "tests" [proptests, unittests]

proptests :: TestTree
proptests = testGroup "property tests" [ testSortSchedules ]

arbitraryTimeOfDay :: Gen TimeOfDay
arbitraryTimeOfDay = TimeOfDay <$> choose (0, 23) <*> choose (0, 59) <*>
        (fromRational . toRational <$> choose (0 :: Double, 60))

arbitraryStop :: Gen Stop
arbitraryStop = Stop <$> arbitrary <*> arbitrary

-- | newtype declaration which wraps the schedule item to avoid orphaned
-- instances warning if we'd just implement the Arbitrary instance for
-- ScheduleItems here
--
newtype ArbitraryScheduleItem = ArbitraryScheduleItem
    { unArbitrary :: ScheduleItem
    } deriving (Show)

instance Arbitrary ArbitraryScheduleItem where
    arbitrary = ArbitraryScheduleItem <$> do
        schedDepTime <- arbitraryTimeOfDay
        delay <- arbitrary
        trip <- arbitrary
        s <- arbitraryStop
        name <- arbitrary
        stype <- elements [CANCELED, ADDED, SCHEDULED]
        return $ ScheduleItem { tripId = trip
                              , stop = s
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
        (\schedule -> propOrderedSchedule $ sortSchedules  $ unwrapScheduleItems schedule)

unwrapScheduleItems :: [ArbitraryScheduleItem] -> [ScheduleItem]
unwrapScheduleItems xs = unArbitrary <$> xs

propOrderedSchedule :: [ScheduleItem] -> Bool
propOrderedSchedule [] = True
propOrderedSchedule [_] = True
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
  output <- capture_ $ printSchedule [] (defaultScheduleConfig $ TimeOfDay 7 7 7)
  output @?= "No services for the next 30min"

makeTest ::
  (TestName, [ScheduleItem], ScheduleConfig, String)
  -> TestTree
makeTest (name, input, cfg, expected) = testCase name $ do
  output <- capture_ $ printSchedule input cfg
  output @?= expected

testFormatScheduleItem ::
  TestTree
testFormatScheduleItem =
    testGroup "formates schedule item" $
    makeTest <$>
    [ ( "punctual"
      , [punctual]
      , (defaultScheduleConfig $ TimeOfDay 7 45 0)
      , "Punctual 5min 07:50:00  ")
    , ( "punctual with walking delay"
      , [ ScheduleItem
          { tripId = "."
          , stop = Stop
            { stopIdentifier = "."
            , stopWalktime = 2
            }
          , serviceName = "Punctual"
          , scheduledDepartureTime = TimeOfDay 7 50 0
          , departureDelay = 0
          , departureTime = TimeOfDay 7 50 0
          , scheduleType = SCHEDULED
          }]
      , (defaultScheduleConfig $ TimeOfDay 7 45 0)
      , "Punctual 3min 07:50:00  ")
    , ( "running late"
      , [runningLate]
      , (defaultScheduleConfig $ TimeOfDay 7 45 0)
      , "!Running Late 6min 07:51:00 +04:40 ")
    , ( "running late + walking delay"
      , [ ScheduleItem
          { tripId = "."
          , stop = Stop
            { stopIdentifier = "."
            , stopWalktime = 2
            }
          , serviceName = "Running Late"
          , scheduledDepartureTime = TimeOfDay 7 50 0
          , departureDelay = 280
          , departureTime = TimeOfDay 7 51 0
          , scheduleType = SCHEDULED
          }]
      , (defaultScheduleConfig $ TimeOfDay 7 45 0)
      , "!Running Late 4min 07:51:00 +04:40 ")
    , ( "running ahead"
      , [runningAhead]
      , (defaultScheduleConfig $ TimeOfDay 7 45 0)
      , "!Running Ahead 5min 07:50:00 -04:20 ")
    , ( "running ahead + walk delay"
      , [ ScheduleItem
          { tripId = "."
          , stop = Stop
            { stopIdentifier = "."
            , stopWalktime = 2
            }
          , serviceName = "Running Ahead"
          , scheduledDepartureTime = TimeOfDay 7 50 0
          , departureDelay = -260
          , departureTime = TimeOfDay 7 50 0  -- not consistent with delay
          , scheduleType = SCHEDULED
          }]
      , (defaultScheduleConfig $ TimeOfDay 7 45 0)
      , "!Running Ahead 3min 07:50:00 -04:20 ")
    , ( "custom template"
      , [runningAhead]
      , (ScheduleConfig (TimeOfDay 7 45 0) "$serviceName$")
      , "Running Ahead")]
  where
    punctual =
        ScheduleItem
        { tripId = "."
        , stop = Stop
          { stopIdentifier = "."
          , stopWalktime = 0
          }
        , serviceName = "Punctual"
        , scheduledDepartureTime = TimeOfDay 7 50 0
        , departureDelay = 0
        , departureTime = TimeOfDay 7 50 0
        , scheduleType = SCHEDULED
        }
    runningAhead =
        ScheduleItem
        { tripId = "."
        , stop = Stop
          { stopIdentifier = "."
          , stopWalktime = 0
          }
        , serviceName = "Running Ahead"
        , scheduledDepartureTime = TimeOfDay 7 50 0
        , departureDelay = -260
        , departureTime = TimeOfDay 7 50 0  -- not consistent with delay
        , scheduleType = SCHEDULED
        }
    runningLate =
        ScheduleItem
        { tripId = "."
        , stop = Stop
          { stopIdentifier = "."
          , stopWalktime = 0
          }
        , serviceName = "Running Late"
        , scheduledDepartureTime = TimeOfDay 7 50 0
        , departureDelay = 280
        , departureTime = TimeOfDay 7 51 0
        , scheduleType = SCHEDULED
        }

testMinutesToDeparture ::
  TestTree
testMinutesToDeparture =
    testGroup "calculates right delay" $
    (\(n,minutes,expected) ->
          testCase n (minutes @?= expected)) <$>
    [ ("simple", minutesToDeparture item (TimeOfDay 7 45 0), 6)
    , ("departure in past", minutesToDeparture item (TimeOfDay 7 52 0), -1)]
  where
    item =
        ScheduleItem
        { tripId = "7136402-BT2015-04_FUL-Weekday-00"
        , stop = Stop
          { stopIdentifier = "10795"
          , stopWalktime = 0
          }
        , serviceName = "Test Service"
        , scheduledDepartureTime = TimeOfDay 7 50 0
        , departureDelay = 60
        , departureTime = TimeOfDay 7 51 0
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
                 , stop = Stop
                   { stopIdentifier = "_"
                   , stopWalktime = 0
                   }
                 , serviceName = "_"
                 , scheduledDepartureTime = TimeOfDay 0 0 0
                 , departureDelay = 40
                 , departureTime = TimeOfDay 0 0 0
                 , scheduleType = SCHEDULED
                 })
          , Just "+40s")
        , ( "minute late"
          , (humanReadableDelay
                 ScheduleItem
                 { tripId = "_"
                 , stop = Stop
                   { stopIdentifier = "_"
                   , stopWalktime = 0
                   }
                 , serviceName = "_"
                 , scheduledDepartureTime = TimeOfDay 0 0 0
                 , departureDelay = 60
                 , departureTime = TimeOfDay 0 0 0
                 , scheduleType = SCHEDULED
                 })
          , Just "+01:00")
        , ( "minutes late"
          , (humanReadableDelay
                 ScheduleItem
                 { tripId = "_"
                 , stop = Stop
                   { stopIdentifier = "_"
                   , stopWalktime = 0
                   }
                 , serviceName = "_"
                 , scheduledDepartureTime = TimeOfDay 0 0 0
                 , departureDelay = 455
                 , departureTime = TimeOfDay 0 0 0
                 , scheduleType = SCHEDULED
                 })
          , Just "+07:35")
        , ( "seconds ahead"
          , (humanReadableDelay
                 ScheduleItem
                 { tripId = "_"
                 , stop = Stop
                   { stopIdentifier = "_"
                   , stopWalktime = 0
                   }
                 , serviceName = "_"
                 , scheduledDepartureTime = TimeOfDay 0 0 0
                 , departureDelay = -20
                 , departureTime = TimeOfDay 0 0 0
                 , scheduleType = SCHEDULED
                 })
          , Just "-20s")
        , ( "minutes ahead"
          , (humanReadableDelay
                 ScheduleItem
                 { tripId = "_"
                 , stop = Stop
                   { stopIdentifier = "_"
                   , stopWalktime = 0
                   }
                 , serviceName = "_"
                 , scheduledDepartureTime = TimeOfDay 0 0 0
                 , departureDelay = -230
                 , departureTime = TimeOfDay 0 0 0
                 , scheduleType = SCHEDULED
                 })
          , Just "-03:50")]
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
             schedule <- getSchedule tmpfile scode timespec 3
             schedule @?= expected)


data TestInput = TestInput { testName :: String
                           , csvdatadirectory :: String
                           , stop' :: Stop
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
      , stop' = Stop
        { stopIdentifier = "600029"
        , stopWalktime = 0
        }
      , now = TimeSpec (TimeOfDay 8 5 0) (fromGregorian 2015 2 7)
      , testExpectedSchedule = []
      }
    , TestInput
      { testName = "no departure because time is past all scheduled services"
      , csvdatadirectory = "regular"
      , stop' = Stop
        { stopIdentifier = "600029"
        , stopWalktime = 0
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
        }
      , now = TimeSpec (TimeOfDay 1 0 0) (fromGregorian 2013 2 4)
      , testExpectedSchedule = [ ScheduleItem
                                 { tripId = "1"
                                 , stop = Stop
                                   { stopIdentifier = "600029"
                                   , stopWalktime = 0
                                   }
                                 , serviceName = "66 Graveyard Express"
                                 , scheduledDepartureTime = TimeOfDay 1 1 0
                                 , departureDelay = 0
                                 , departureTime = TimeOfDay 1 1 0
                                 , scheduleType = SCHEDULED
                                 }
                               , ScheduleItem
                                 { tripId = "2"
                                 , stop = Stop
                                   { stopIdentifier = "600029"
                                   , stopWalktime = 0
                                   }
                                 , serviceName = "66 Graveyard Express"
                                 , scheduledDepartureTime = TimeOfDay 2 1 0
                                 , departureDelay = 0
                                 , departureTime = TimeOfDay 2 1 0
                                 , scheduleType = SCHEDULED
                                 }]
      }
    , TestInput
      { testName = "additional temp scheduled service"
      , csvdatadirectory = "tempservice"
      , stop' = Stop
        { stopIdentifier = "600029"
        , stopWalktime = 0
        }
      , now = TimeSpec (TimeOfDay 8 5 0) (fromGregorian 2015 1 28)
      , testExpectedSchedule = [ ScheduleItem
                                 { tripId = "QF0815-00"
                                 , stop = Stop
                                   { stopIdentifier = "600029"
                                   , stopWalktime = 0
                                   }
                                 , serviceName = "66 not relevant"
                                 , scheduledDepartureTime = TimeOfDay 8 5 0
                                 , departureDelay = 0
                                 , departureTime = TimeOfDay 8 5 0
                                 , scheduleType = SCHEDULED
                                 }
                               , ScheduleItem
                                 { tripId = "QF0815-00-Ekka"
                                 , stop = Stop
                                   { stopIdentifier = "600029"
                                   , stopWalktime = 0
                                   }
                                 , serviceName = "66 not relevant"
                                 , scheduledDepartureTime = TimeOfDay 8 5 33
                                 , departureDelay = 0
                                 , departureTime = TimeOfDay 8 5 33
                                 , scheduleType = SCHEDULED
                                 }
                               , ScheduleItem
                                 { tripId = "QF0815-00"
                                 , stop = Stop
                                   { stopIdentifier = "600029"
                                   , stopWalktime = 0
                                   }
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
      , stop' = Stop
        { stopIdentifier = "600029"
        , stopWalktime = 0
        }
      , now = TimeSpec (TimeOfDay 8 5 0) (fromGregorian 2015 2 4)
      , testExpectedSchedule = [ ScheduleItem
                                 { tripId = "QF0815-00-Ekka"
                                 , stop = Stop
                                   { stopIdentifier = "600029"
                                   , stopWalktime = 0
                                   }
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
