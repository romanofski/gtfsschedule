module CSV.Import where

import qualified CSV.Route as CSVRoute
import qualified CSV.Trip as CSVTrip
import qualified CSV.Calendar as CSVCalendar
import qualified CSV.Stop as CSVStop
import qualified CSV.StopTime as CSVStopTime

import qualified Data.ByteString.Lazy as B
import Data.Csv.Streaming (decodeByName, Records)
import Data.Csv (FromNamedRecord)

import qualified Database as DB
import qualified Data.Text as T


parseCSV ::
  FromNamedRecord a
  => B.ByteString
  -> Either String (Records a)
parseCSV contents =
  case decodeByName contents of
    Left errmsg -> Left errmsg
    Right (_, r) -> Right r

runImport :: IO ()
runImport = do
  DB.runDBWithoutLogging dbpath DB.prepareDatabaseForUpdate
  importCSV dbpath "data/routes.txt" CSVRoute.insertIntoDB
  importCSV dbpath "data/trips.txt" CSVTrip.insertIntoDB
  importCSV dbpath "data/calendar.txt" CSVCalendar.insertIntoDB
  importCSV dbpath "data/stops.txt" CSVStop.insertIntoDB
  importCSV dbpath "data/stop_times.txt" CSVStopTime.insertIntoDB
  return ()
    where dbpath = T.pack ":memory:"

importCSV dbpath filepath func = do
  contents' <- B.readFile filepath
  let (Right trips) = parseCSV contents' -- :-O
  mapM_ (\x -> DB.runDBWithLogging dbpath $ func x) (trips)
