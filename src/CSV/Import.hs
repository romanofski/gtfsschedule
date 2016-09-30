module CSV.Import where

import qualified CSV.Route as CSVRoute
import qualified CSV.Trip as CSVTrip
import qualified CSV.Calendar as CSVCalendar
import qualified CSV.Stop as CSVStop
import qualified CSV.StopTime as CSVStopTime

import qualified Data.ByteString.Lazy as B
import Data.Csv.Streaming (decodeByName)
import Data.Csv (FromNamedRecord)

import Data.Int (Int64)
import Control.Monad (liftM)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Resource (MonadResource)
import Database.Esqueleto (persistBackend, PersistValue(..))
import qualified Database as DB
import qualified Data.Text as T
import qualified Database.Persist.Sqlite as Sqlite


runImport :: IO ()
runImport = DB.runDBWithoutLogging (T.pack ":memory:") $ do
  DB.prepareDatabaseForUpdate
  importCSV ("data/routes.txt", CSVRoute.prepareSQL, CSVRoute.convertToValues)
  importCSV ("data/stops.txt", CSVStop.prepareSQL, CSVStop.convertToValues)
  importCSV ("data/trips.txt", CSVTrip.prepareSQL, CSVTrip.convertToValues)
  importCSV ("data/calendar.txt", CSVCalendar.prepareSQL, CSVCalendar.convertToValues)
  importCSV ("data/stop_times.txt", CSVStopTime.prepareSQL, CSVStopTime.convertToValues)

importCSV ::
  (FromNamedRecord a, MonadIO m, MonadResource m)
  => (String, T.Text, (a -> [PersistValue]))
  -> ReaderT Sqlite.SqlBackend m ()
importCSV (filepath, sql, convertfunc) = do
  liftIO $ print filepath
  contents <- liftIO $ B.readFile filepath
  case decodeByName contents of
    Left errmsg -> liftIO $ print errmsg
    Right (_, records) -> do
      stmt <- prepareStmt sql
      mapM_ (\x -> rawInsert stmt $ convertfunc x) records
      liftIO $ Sqlite.stmtFinalize stmt
      return ()

prepareStmt ::
  (MonadIO m)
  => T.Text
  -> ReaderT Sqlite.SqlBackend m Sqlite.Statement
prepareStmt sql = do
  conn <- persistBackend `liftM` ask
  stmt <- liftIO $ Sqlite.connPrepare conn sql
  return stmt

rawInsert ::
  (MonadIO m, MonadResource m)
  => Sqlite.Statement
  -> [Sqlite.PersistValue]
  -> ReaderT Sqlite.SqlBackend m Int64
rawInsert stmt vals = do
  res <- liftIO $ Sqlite.stmtExecute stmt vals
  liftIO $ Sqlite.stmtReset stmt
  return res
