module CSV.Import (createNewDatabase) where

import qualified CSV.Route as CSVRoute
import qualified CSV.Trip as CSVTrip
import qualified CSV.Calendar as CSVCalendar
import qualified CSV.Stop as CSVStop
import qualified CSV.StopTime as CSVStopTime

import qualified Data.ByteString.Lazy as B
import Data.Csv.Streaming (decodeByName)
import Data.Csv (FromNamedRecord)

import Network.HTTP.Conduit (responseBody, http, parseRequest, tlsManagerSettings, newManager)
import Data.Conduit (($$+-))
import Data.Conduit.Binary (sinkFile)

import Data.Int (Int64)
import Control.Monad (liftM)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Database.Esqueleto (persistBackend, PersistValue(..))
import qualified Database as DB
import qualified Database.Persist.Sqlite as Sqlite
import qualified Data.Text as T
import qualified Codec.Archive.Zip as Zip
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (renameFile)


datasetZipFilename :: String
datasetZipFilename = "StaticDataset.zip"

createNewDatabase ::
  String
  -> IO ()
createNewDatabase url = withSystemTempDirectory "NewGTFSDB" $ \x -> do
  newDB <- downloadStaticDataset url x >>= unzipDataset >> runImport
  currentDB <- DB.userDatabaseFile
  renameFile newDB currentDB

-- | Downloads new data set to systems temp directory
-- TODO: file is assumed to be a zip file
--
downloadStaticDataset ::
  String
  -> FilePath
  -> IO (FilePath)
downloadStaticDataset url downloadDir = runResourceT $ do
  manager <- liftIO $ newManager tlsManagerSettings
  request <- liftIO $ parseRequest url
  response <- http request manager
  responseBody response $$+- sinkFile downloadfp
  return downloadDir
    where downloadfp = (concat [downloadDir, "/", datasetZipFilename])


unzipDataset ::
  FilePath
  -> IO (FilePath)
unzipDataset downloaddir = do
  contents <- B.readFile zipfile
  Zip.extractFilesFromArchive [Zip.OptDestination downloaddir] (Zip.toArchive contents)
  return downloaddir
    where zipfile = concat [downloaddir, "/", datasetZipFilename]


-- | runs the import against the given database
--
runImport :: IO (FilePath)
runImport = do
  currentDBFile <- DB.userDatabaseFile
  let newDBFile = T.pack $ currentDBFile ++ ".new"
  DB.runDBWithoutLogging newDBFile $ do
    Sqlite.runMigration DB.migrateAll
    DB.prepareDatabaseForUpdate DB.Started
    importCSV ("routes.txt", CSVRoute.prepareSQL, CSVRoute.convertToValues)
    importCSV ("stops.txt", CSVStop.prepareSQL, CSVStop.convertToValues)
    importCSV ("trips.txt", CSVTrip.prepareSQL, CSVTrip.convertToValues)
    importCSV ("calendar.txt", CSVCalendar.prepareSQL, CSVCalendar.convertToValues)
    importCSV ("stop_times.txt", CSVStopTime.prepareSQL, CSVStopTime.convertToValues)
    DB.prepareDatabaseForUpdate DB.Finished
    return $ T.unpack newDBFile

importCSV ::
  (FromNamedRecord a, MonadIO m, MonadResource m)
  => (String, T.Text, (a -> [PersistValue]))
  -> ReaderT Sqlite.SqlBackend m ()
importCSV (filepath, sql, convertfunc) = do
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
