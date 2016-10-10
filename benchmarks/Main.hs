{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import CSV.Import (runImport)
import qualified Data.Text as T

main :: IO ()
main = runImport (T.pack ":memory:")
