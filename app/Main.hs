module Main where

import Options.Applicative.Builder ( switch
                                   , long
                                   , help
                                   , (<>)
                                   , metavar
                                   , strOption
                                   , option
                                   , fullDesc
                                   , progDesc
                                   , header
                                   , info
                                   , auto)
import Options.Applicative.Types (Parser)
import Options.Applicative.Extra ( execParser
                                 , helper)
import Schedule ( printSchedule )
import qualified Data.ByteString.Lazy as B


data Options = Options {stationID :: String
                       , stationFilePath :: FilePath
                       , delayInMinutes :: Integer
                       }

optionParser ::
  Parser Options
optionParser = Options
               <$> strOption
               ( long "stationID"
                 <> metavar "sID"
                 <> help "Station ID to show the schedule for")
               <*> strOption
               ( long "stopTimesTxt"
                 <> metavar "FILEPATH"
                 <> help "filepath to stop_times.txt csv data to read from")
               <*> option auto
               ( long "walktime"
                 <> help "Time to reach the stop. Will be added to the current time to allow arriving at the stop on time.")


runSchedule :: Options -> IO ()
runSchedule (Options sID fp delay) = do
  contents <- B.readFile fp
  printSchedule sID delay contents

main :: IO ()
main = execParser opts >>= runSchedule
  where opts = info (helper <*> optionParser)
               ( fullDesc
                 <> progDesc "shows schedule times GTFS style"
                 <> header "gtfs - a gtfs enabled brisbane transport schedule")
