module Main where

import CLI.Main (runCLI)

import Data.Version (showVersion)
import Paths_gtfsschedule (version)


programHeader :: String
programHeader =
  "gtfsschedule - Be on time for your next public transport service (v. " ++ showVersion version ++ ")"


main :: IO ()
main = runCLI programHeader "Shows schedule of departing vehicles based on static GTFS data."
