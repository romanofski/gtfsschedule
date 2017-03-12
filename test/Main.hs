module Main where

import TestRealtime (feedTests)
import TestCSVImport (importTests)
import TestUpdate (updateTests)
import TestProperty (proptests)
import TestSchedule (scheduleTests)

import Test.Tasty (defaultMain, TestTree, testGroup)

tests ::
  TestTree
tests = testGroup "tests" [proptests, unittests]

-- unit tests
--
unittests :: TestTree
unittests =
    testGroup "unit tests" [feedTests, importTests, updateTests, scheduleTests]

main ::
  IO ()
main = defaultMain tests
