module Main where

import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)
import qualified Data.ByteString.Lazy as Lazy
import Com.Google.Transit.Realtime.TripUpdate.StopTimeUpdate

import Lib (getSchedule)

tests ::
  TestTree
tests = testGroup "unit tests" [unitTests]

unitTests ::
  TestTree
unitTests = testGroup "schedule tests"
            [ testgetSchedule ]

testHasDeparture ::
  TestTree
testHasDeparture =
  testCase "correctly shows departure" $ assertBool "expected stopid not empty" departure
  where departure = case getSchedule getGTFSFeed of
          Nothing -> Nothing
          Just msg -> getDeparture msg

-- helpers
--
getGTFSFeed :: IO Lazy.Bytestring
getGTFSFeed = readFile "data/SEQ"

main ::
  IO ()
main = defaultMain tests
