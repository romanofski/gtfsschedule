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
module TestUpdate (updateTests) where

import           Fixtures             (withConcurrentTCPServer)
import           GTFS.Realtime.Update (Error (..), isCurrent, isDatasetUpToDate)

import           Data.Time.Calendar   (fromGregorian)
import           Test.Tasty           (TestTree, testGroup)
import           Test.Tasty.HUnit     (testCase, (@?=))

import           Data.Conduit         (yield, ($$))
import           Data.Conduit.Network (AppData, appSink)

import qualified Data.Text            as T

updateTests ::
  TestTree
updateTests = testGroup "update tests"
            [ testDatabaseUpToDate
            , testDatabaseOutdated
            , testNoModifiedHeaders
            , testOnlyErrorOnUnavailableServer
            ]

testOnlyErrorOnUnavailableServer :: TestTree
testOnlyErrorOnUnavailableServer = testCase "only error on unavailable server" $ do
      result <- isDatasetUpToDate ("http://127.0.0.1:39299") (fromGregorian 2016 10 26) isCurrent
      result @?= Left (Error "Problem communicating with server. Received empty headers.")

testDatabaseUpToDate :: TestTree
testDatabaseUpToDate =
    testCase "database is up to date" $
    withConcurrentTCPServer withHTTPDataHeadersOnly $ \port -> do
      result <- isDatasetUpToDate (T.pack $ "http://127.0.0.1:" ++ show port) (fromGregorian 2016 10 26) isCurrent
      result @?= Right True

testDatabaseOutdated :: TestTree
testDatabaseOutdated =
    testCase "database is outdated" $
    withConcurrentTCPServer withHTTPDataHeadersOnly $ \port -> do
      result <- isDatasetUpToDate (T.pack $ "http://127.0.0.1:" ++ show port) (fromGregorian 2016 10 23) isCurrent
      result @?= Right False

withHTTPDataHeadersOnly :: AppData -> IO ()
withHTTPDataHeadersOnly appData = src $$ appSink appData
  where
    src = yield "HTTP/1.1 200 OK\r\nLast-Modified: Tue, 25 Oct 2016 01:51:58 GMT\r\nContent-Type: text/plain\r\n\r\nTest"

testNoModifiedHeaders :: TestTree
testNoModifiedHeaders =
    testCase "no modified headers results in error" $
    withConcurrentTCPServer withHTTPDataNoModifiedHeader $ \port -> do
      result <- isDatasetUpToDate (T.pack $ "http://127.0.0.1:" ++ show port) (fromGregorian 2016 10 23) isCurrent
      result @?= Left (Error "Couldn't determine last modification date from server headers: [(\"Content-Type\",\"text/plain\")]")

withHTTPDataNoModifiedHeader :: AppData -> IO ()
withHTTPDataNoModifiedHeader appData = src $$ appSink appData
  where
    src = yield "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\nTest"
