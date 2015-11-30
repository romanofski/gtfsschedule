module Main where

import System.Environment (getArgs)
import Schedule (printSchedule)
import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
  (fp:_) <- getArgs
  contents <- B.readFile fp
  printSchedule "600029" contents
