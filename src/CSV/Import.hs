{-
Copyright (C) - 2017 Róman Joost <roman@bromeco.de>

This file is part of gtfsschedule.

gtfsschedule is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

gtfsschedule is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with gtfsschedule.  If not, see <http://www.gnu.org/licenses/>.
-}
{- | This module provides functions to download, unpack and import the static
schedule from the provided CSV files.

GTFS traffic data comes with a static set of CSV files, apart from the realtime
data providing changes to the static schedule.

See also: https://developers.google.com/transit/gtfs/reference/
-}
module CSV.Import (createNewDatabase, runImport) where

import qualified CSV.Import.Calendar          as CSVCalendar
import qualified CSV.Import.Route             as CSVRoute
import qualified CSV.Import.Stop              as CSVStop
import qualified CSV.Import.StopTime          as CSVStopTime
import qualified CSV.Import.Trip              as CSVTrip

import qualified GTFS.Database                as DB

import qualified Data.ByteString.Lazy         as B
import           Data.Csv                     (FromNamedRecord)
import           Data.Csv.Streaming           (decodeByName)

import           Data.Conduit                 (($$+-), newResumableSource)
import           Data.Conduit.Binary          (sinkFile)
import           Data.Foldable                (mapM_)
import           Network.HTTP.Client.Conduit  (defaultManagerSettings)
import           Network.HTTP.Client          (parseRequest)
import           Network.HTTP.Conduit         (http, newManager, responseBody)
import           Prelude                      hiding (mapM_)

import qualified Codec.Archive.Zip            as Zip
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Trans.Reader   (ReaderT)
import           Control.Monad.Trans.Resource (MonadResource, runResourceT)
import qualified Data.Text                    as T
import           Database.Esqueleto           (PersistValue (..))
import qualified Database.Persist.Sqlite      as Sqlite
import qualified Filesystem.Path.CurrentOS    as Path
import           System.Directory             (renameFile)
import           System.Directory             (createDirectoryIfMissing)
import           System.IO                    (hPutStr, stderr)
import           System.IO.Temp               (withSystemTempDirectory)


datasetZipFilename :: String
datasetZipFilename = "StaticDataset.zip"

-- | Creates a new database by fetching a GTFS static dataset from the given URL
-- The sequence of actions is as follows:
--
--  * Download the static dataset as a zip file
--
--  * extract all CSV files
--
--  * create a new, temporary database in which we import necessary CSV data
--
--  * rename current database with the newly created one
--
-- For each step a progress is reported to stderr
--
createNewDatabase
    :: String  -- ^ URL to the static dataset
    -> FilePath  -- ^ Path to the users database file (if it exists)
    -> IO ()
createNewDatabase url currentDBFile = withSystemTempDirectory "NewGTFSDB" $ \x -> do
  let newDBFile = currentDBFile ++ ".new"
  ensureUserDatabaseDir (Path.fromText $ T.pack currentDBFile)
  _ <- printProgress 2 ""
  downloadStaticDataset url x >>= printProgress 3 >>= unzipDataset >>= printProgress 5 >>= runImport newDBFile
  _ <- printProgress 8 currentDBFile >>= renameFile newDBFile >> printProgress 10 ""
  return ()

-- | shows a progress bar to indicate overall import progress
-- Kudos to: http://stackoverflow.com/questions/8953636/simple-progress-indication-in-console
--
printProgress :: Int -> a -> IO (a)
printProgress progress x = do
  putProgress $ drawProgressBar 50 (fromIntegral progress / 10)
  return x

putProgress :: String -> IO ()
putProgress s = hPutStr stderr $ "\r\ESC[K" ++ s

drawProgressBar :: Int -> Rational -> String
drawProgressBar width progress = "[" ++ replicate dots '.' ++ replicate spaces ' ' ++ "]"
  where dots = round (progress * fromIntegral width)
        spaces = width - dots

-- | Prepares the user database directory if it doesn't exist
--
ensureUserDatabaseDir ::
  Path.FilePath
  -> IO ()
ensureUserDatabaseDir userdbfile =
  case Path.toText $ Path.directory userdbfile of
    Right dbdir -> createDirectoryIfMissing True (T.unpack dbdir)
    Left path -> ioError $ userError ("Path has invalid encoding: " ++ T.unpack path)

-- | Downloads new data set to systems temp directory
--
-- TODO: file is assumed to be a zip file
--
downloadStaticDataset ::
  String
  -> FilePath
  -> IO (FilePath)
downloadStaticDataset url downloadDir = runResourceT $ do
  manager <- liftIO $ newManager defaultManagerSettings
  request <- liftIO $ parseRequest url
  response <- http request manager
  (newResumableSource $ responseBody response) $$+- sinkFile downloadfp
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
-- CSV file names have to conform with Googles GTFS reference, e.g. (routes.txt, instead of routes.csv)
--
runImport ::
  FilePath  -- ^ path to new SQLite database file
  -> FilePath  -- ^ directory in which CSV files are found
  -> IO ()
runImport newDBFile downloaddir = DB.runDBWithoutLogging (T.pack newDBFile) $ do
    _ <- Sqlite.runMigrationSilent DB.migrateAll
    DB.prepareDatabaseForUpdate DB.Started
    importCSV (absolutePath downloaddir "routes.txt", CSVRoute.prepareSQL, CSVRoute.convertToValues)
    importCSV (absolutePath downloaddir "stops.txt", CSVStop.prepareSQL, CSVStop.convertToValues)
    importCSV (absolutePath downloaddir "trips.txt", CSVTrip.prepareSQL, CSVTrip.convertToValues)
    importCSV (absolutePath downloaddir "calendar.txt", CSVCalendar.prepareSQL, CSVCalendar.convertToValues)
    importCSV (absolutePath downloaddir "stop_times.txt", CSVStopTime.prepareSQL, CSVStopTime.convertToValues)
    DB.addDatabaseIndices
    DB.prepareDatabaseForUpdate DB.Finished
      where absolutePath path file = concat [path, "/", file]

importCSV ::
  (FromNamedRecord a, MonadIO m, MonadResource m)
  => (String, T.Text, (a -> [PersistValue]))
  -> ReaderT Sqlite.SqlBackend m ()
importCSV (filepath, sql, convertfunc) = do
  contents <- liftIO $ B.readFile filepath
  case decodeByName contents of
    Left errmsg -> liftIO $ print errmsg
    Right (_, records) -> do
      stmt <- DB.prepareStmt sql
      mapM_ (\x -> DB.rawInsert stmt $ convertfunc x) records
      liftIO $ Sqlite.stmtFinalize stmt
      return ()
