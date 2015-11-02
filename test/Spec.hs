module Main where

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import qualified Data.ByteString.Lazy as B
import Data.Time.LocalTime (TimeOfDay(..))

import Schedule (parseCSV, filterRecords, StopTime(..))

tests ::
  TestTree
tests = testGroup "unit tests" [unitTests]

unitTests ::
  TestTree
unitTests = testGroup "schedule tests"
            [ testParsesCSVWithInvalidData
            , testParsesCSV
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
    Right r -> return $ filterRecords r

main ::
  IO ()
main = defaultMain tests
