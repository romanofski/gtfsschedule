module Main where

import System.Environment (getArgs)
import Schedule (parseCSV, filterRecords)
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
  (fp:_) <- getArgs
  contents <- B.readFile fp
  records <- parseCSV contents
  case records of
    Left err -> print err
    Right r -> print $ filterRecords r
