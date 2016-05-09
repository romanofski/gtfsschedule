module Main where

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool)
import Data.Time.LocalTime (TimeOfDay(..))
import Data.Time.Clock (secondsToDiffTime)
import Data.Time.Calendar (fromGregorian)
import Database.Persist.Sqlite (runMigrationSilent)
import Database.Persist (insert, entityVal)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

import Schedule ( parseCSV
                , filterRecords
                , StopTime(..)
                , isInvalidStop
                , isInvalidWeekday
                , isInvalidDepartureTime
                , minutesToDeparture
                )
import qualified Database as DB

tests ::
  TestTree
tests = testGroup "unit tests" [unitTests]

unitTests ::
  TestTree
unitTests = testGroup "schedule tests"
            [ testParsesCSVWithInvalidData
            , testParsesCSV
            , testIgnoresRecords
            , testMinutesToDepartureWorks
            , testGetsNextDepartures
            ]

testGetsNextDepartures ::
  TestTree
testGetsNextDepartures = testCase "check next departures" $ do
  stops <- DB.runDBWithLogging (T.pack ":memory:") getStops
  let l = DB.stopTimeStop . entityVal <$> stops
  assertEqual "expected one stop time" ["600029"] l
  where getStops = do
          _ <- runMigrationSilent DB.migrateAll
          let serviceId = "QF0815"
          tID <- insert DB.Trip { DB.tripRouteId = "."
                                , DB.tripServiceId = serviceId
                                , DB.tripHeadsign = Nothing
                                , DB.tripDirectionId = Nothing
                                , DB.tripShortName = Nothing
                                , DB.tripBlockId = Nothing
                                , DB.tripShapeId = Nothing
                                , DB.tripWheelchairAccessible = Nothing
                                , DB.tripBikesAllowed = Nothing
                                }
          _ <- insert DB.Calendar { DB.calendarServiceId = serviceId
                                  , DB.calendarMonday = False
                                  , DB.calendarTuesday = False
                                  , DB.calendarWednesday = True
                                  , DB.calendarThursday = False
                                  , DB.calendarFriday = False
                                  , DB.calendarSaturday = False
                                  , DB.calendarSunday = False
                                  , DB.calendarStartDate = fromGregorian 2011 1 1
                                  , DB.calendarEndDate = fromGregorian 2020 1 1
                                  }
          _ <- insert DB.StopTime { DB.stopTimeTrip = tID
                                  , DB.stopTimeArrivalTime = TimeOfDay 8 02 00
                                  , DB.stopTimeDepartureTime = TimeOfDay 8 05 00
                                  , DB.stopTimeStop = "600029"
                                  , DB.stopTimeStopSequence = "1"
                                  , DB.stopTimeStopHeadsign = Nothing
                                  , DB.stopTimePickupType = Nothing
                                  , DB.stopTimeDropOffType = Nothing
                                  , DB.stopTimeShapeDistTravel = Nothing
                                  , DB.stopTimeTimepoint = Nothing
                                  }
          DB.getNextDepartures "600029" (TimeOfDay 8 05 00) (fromGregorian 2015 1 7)

testParsesCSVWithInvalidData ::
  TestTree
testParsesCSVWithInvalidData =
  testCase "correctly ignores invalid csv data" $ do
    parsed <- getFilteredStopTimes invalidCSVStopTimes
    assertEqual "expecting one record" expected parsed
    where expected = [ StopTime { trip_id = "3-QR2015"
                                 , arrival_time = TimeOfDay 8 03 00
                                 , departure_time = TimeOfDay 8 03 00
                                 , stop_id = "600029"
                                 , stop_sequence = 20
                                 , pickup_type = 0
                                 , drop_off_type = 0}
                     ]

testParsesCSV ::
  TestTree
testParsesCSV =
  testCase "parses csv data successfully" $ do
    parsed <- getFilteredStopTimes validCSVStopTimes
    assertEqual "expecting two records" expected parsed
     where expected = [ StopTime { trip_id = "3-QR2015"
                                 , arrival_time = TimeOfDay 8 03 00
                                 , departure_time = TimeOfDay 8 03 00
                                 , stop_id = "600029"
                                 , stop_sequence = 20
                                 , pickup_type = 0
                                 , drop_off_type = 0}
                      , StopTime { trip_id = "4-QR2015"
                                 , arrival_time = TimeOfDay 8 04 00
                                 , departure_time = TimeOfDay 8 04 00
                                 , stop_id = "600029"
                                 , stop_sequence = 20
                                 , pickup_type = 0
                                 , drop_off_type = 0}
                      ]

testIgnoresRecords ::
  TestTree
testIgnoresRecords =
  testGroup "ignores"
  [ testCase "invalidStop" $ assertBool "false" $ not (isInvalidStop "39" $ head dataFixtures)
  , testCase "invalidStop" $ assertBool "true" $ isInvalidStop "600029" $ head dataFixtures
  , testCase "invalidWeekday" $ assertBool "false" $ not (isInvalidWeekday "Tuesday" $ head dataFixtures)
  , testCase "invalidWeekday" $ assertBool "true" $ isInvalidWeekday "Friday" $ head dataFixtures
  , testCase "invalidDepartureTime" $ assertBool "false" $ not $
    isInvalidDepartureTime (secondsToDiffTime 0) (TimeOfDay 8 05 00) $ head dataFixtures
  , testCase "invalidDepartureTime" $ assertBool "true" $
    isInvalidDepartureTime (secondsToDiffTime 0) (TimeOfDay 8 02 00) $ head dataFixtures
  , testCase "invalidDepartureTime with delay" $ assertBool "false" $ not $
    isInvalidDepartureTime (secondsToDiffTime 5 * 60) (TimeOfDay 8 03 00) $ head dataFixtures
  ]

testMinutesToDepartureWorks ::
  TestTree
testMinutesToDepartureWorks =
  testGroup "minutesToDeparture"
  [ testCase "future" $ assertEqual "expects number" 4 (
      minutesToDeparture (TimeOfDay 8 00 00) (TimeOfDay 8 04 00) 0)
  , testCase "future with seconds" $ assertEqual "expects number" 5 (
      minutesToDeparture (TimeOfDay 8 00 00) (TimeOfDay 8 04 59) 0)
  , testCase "to late" $ assertEqual "expects number" (-2) (
      minutesToDeparture (TimeOfDay 8 05 00) (TimeOfDay 8 03 00) 0)
  ]

dataFixtures :: [StopTime]
dataFixtures = [ StopTime { trip_id = "5529773-QR2015-MTP_Fri-Friday-01-1918"
                          , arrival_time = TimeOfDay 8 03 00
                          , departure_time = TimeOfDay 8 03 00
                          , stop_id = "600029"
                          , stop_sequence = 20
                          , pickup_type = 0
                          , drop_off_type = 0}
               , StopTime { trip_id = "5529773-QR2015-MTP_Tue-Tuesday-01-1918"
                          , arrival_time = TimeOfDay 8 04 00
                          , departure_time = TimeOfDay 8 04 00
                          , stop_id = "600029"
                          , stop_sequence = 20
                          , pickup_type = 0
                          , drop_off_type = 0}
               ]

-- helpers
--
invalidCSVStopTimes :: IO B.ByteString
invalidCSVStopTimes = B.readFile "test/data/invalid_stop_times.txt"

validCSVStopTimes :: IO B.ByteString
validCSVStopTimes = B.readFile "test/data/valid_stop_times.txt"

getFilteredStopTimes f = do
  c <- f
  parsed <- parseCSV c
  case parsed of
    Left _ -> return []
    Right r -> return $ filterRecords (\x -> stop_id x == "600029") r

main ::
  IO ()
main = defaultMain tests
