module Main where

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase, assertBool)
import qualified Data.ByteString.Lazy as B
import Data.Time.LocalTime (TimeOfDay(..), TimeZone(..))
import Data.Time.Clock (UTCTime(..))
import Data.Time.Calendar (Day(..))

import Schedule ( parseCSV
                , filterRecords
                , StopTime(..)
                , isInvalidStop
                , isInvalidWeekday
                , isInvalidDepartureTime
                )

tests ::
  TestTree
tests = testGroup "unit tests" [unitTests]

unitTests ::
  TestTree
unitTests = testGroup "schedule tests"
            [ testParsesCSVWithInvalidData
            , testParsesCSV
            , testIgnoresRecords
            ]

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
  , testCase "invalidDepartureTime" $ assertBool "false" $ isInvalidDepartureTime (UTCTime (ModifiedJulianDay 0) 3000) (TimeZone 0 False "test") $ head dataFixtures
  , testCase "invalidDepartureTime" $ assertBool "true" $ isInvalidDepartureTime (UTCTime (ModifiedJulianDay 0) 0) (TimeZone 0 False "test") $ head dataFixtures
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
