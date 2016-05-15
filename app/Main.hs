module Main where

import Options.Applicative.Builder (long
                                   , help
                                   , (<>)
                                   , metavar
                                   , option
                                   , argument
                                   , str
                                   , fullDesc
                                   , progDesc
                                   , header
                                   , info
                                   , auto)
import Options.Applicative (optional)
import Options.Applicative.Types (Parser)
import Options.Applicative.Extra ( execParser
                                 , helper)
import Schedule ( printSchedule )
import Data.Maybe (fromMaybe)


type Station = String
type SQLiteDBPath = FilePath
type WalkTime = Integer

data Options = Options { stationID :: String
                       , sqliteDBFile :: FilePath
                       , walktime :: Maybe Integer
                       }

optionParser ::
  Parser Options
optionParser = Options
               <$> argument str
               ( metavar "filepath"
                 <> help "filepath to sqlite3 database populated with GTFS static data feed")
               <*> argument str
               ( metavar "station"
                 <> help "Station id to show the schedule for")
               <*> (optional $ option auto
               ( long "walktime"
                 <> help "Time to reach the stop. Will be added to the current time to allow arriving at the stop on time."))


runSchedule :: Options -> IO ()
runSchedule (Options fp sID delay) = printSchedule fp sID $ fromMaybe 0 delay

main :: IO ()
main = execParser opts >>= runSchedule
  where opts = info (helper <*> optionParser)
               ( fullDesc
                 <> progDesc "Shows schedule of departing vehicles based on static GTFS data."
                 <> header "gtfs - a gtfs enabled transport schedule")
