#!/usr/bin/env stack
-- stack --resolver lts-4.2 --install-ghc runghc --package=protocol-buffers --package=gtfsschedule
module Main where

import Text.ProtocolBuffers (messageGet)
import qualified Message as FM
import qualified Data.ByteString.Lazy as L

withFeed ::
  FilePath
  -> IO (Maybe FM.FeedMessage)
withFeed fp = do
  contents <- L.readFile fp
  case messageGet contents of
    Left err -> do
      print err
      return Nothing
    Right (fm, _) -> return $ Just fm

main ::
  IO ()
main = do
  fm <- withFeed "test/data/feed.bin"
  print $ show fm
