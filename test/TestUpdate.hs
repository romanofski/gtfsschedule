{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TestUpdate (updateTests) where

import Fixtures (withConcurrentTCPServer)
import Update (isDatasetUpToDate, isCurrent)

import Data.Time.Calendar (fromGregorian)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty (TestTree, testGroup)

import Data.Conduit (($$), yield)
import Data.Conduit.Network (AppData, appSink)

updateTests ::
  TestTree
updateTests = testGroup "update tests"
            [ testDatabaseUpToDate
            , testDatabaseOutdated
            ]

testDatabaseUpToDate :: TestTree
testDatabaseUpToDate =
    testCase "database is up to date" $
    withConcurrentTCPServer withHTTPDataHeadersOnly $ \port -> do
      result <- isDatasetUpToDate ("http://127.0.0.1:" ++ show port) (fromGregorian 2016 10 26) isCurrent
      result @?= Right True

testDatabaseOutdated :: TestTree
testDatabaseOutdated =
    testCase "database is outdated" $
    withConcurrentTCPServer withHTTPDataHeadersOnly $ \port -> do
      result <- isDatasetUpToDate ("http://127.0.0.1:" ++ show port) (fromGregorian 2016 10 23) isCurrent
      result @?= Right False

withHTTPDataHeadersOnly :: AppData -> IO ()
withHTTPDataHeadersOnly appData = src $$ appSink appData
  where
    src = yield "HTTP/1.1 200 OK\r\nLast-Modified: Tue, 25 Oct 2016 01:51:58 GMT\r\nContent-Type: text/plain\r\n\r\nTest"
