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
module Main where

import           TestCSVImport (importTests)
import           TestProperty  (proptests)
import           TestRealtime  (feedTests)
import           TestSchedule  (scheduleTests)
import           TestUpdate    (updateTests)

import           Test.Tasty    (TestTree, defaultMain, testGroup)

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
