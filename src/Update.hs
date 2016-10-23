module Update (isDatasetUpToDate, printWarningForNewDataset, isCurrent) where

import Network.HTTP.Conduit
import Network.HTTP.Types.Header (ResponseHeaders, Header, hLastModified)
import Data.Time.Calendar (Day)
import System.Locale (defaultTimeLocale)
import Data.Time.Format (parseTime)
import Data.List (find)
import System.IO (hPrint, stderr)
import qualified Data.ByteString.Char8 as B


data Error = Error String
  deriving Show

-- | Returns True if the dataset downloadable from given URL is still current
-- Note: uses last modified header to determine if it has recently been updated
--
isDatasetUpToDate ::
  String
  -> Day
  -> (Day -> Day -> Bool)
  -> IO (Either Error Bool)
isDatasetUpToDate url dbmodified f = do
  headers <- getHeadersForDataset url
  let lastmod = getLastModified headers
  case parseLastModified lastmod of
    Just datasetModified -> return $ Right (f datasetModified dbmodified)
    Nothing -> return $ Left (Error ("Couldn't parse last modified header: " ++ show headers))

printWarningForNewDataset ::
  Either Error Bool
  -> IO ()
printWarningForNewDataset (Right False) = print "Note: New dataset available!"
printWarningForNewDataset (Right _) = putStrLn ""
printWarningForNewDataset (Left _) = hPrint stderr "Warning: Couldn't determine if dataset is outdated."

isCurrent ::
  Day
  -> Day
  -> Bool
isCurrent lastModified dbmod = lastModified <= dbmod

--
-- private helpers

getHeadersForDataset ::
  String
  -> IO ResponseHeaders
getHeadersForDataset url = do
  request <- parseUrl ("HEAD " ++ url)
  manager <- newManager conduitManagerSettings
  response <- httpLbs request manager
  return $ responseHeaders response

getLastModified ::
  ResponseHeaders
  -> Maybe Header
getLastModified = find (\(n, _) -> n == hLastModified)

parseLastModified ::
  Maybe Header
  -> Maybe Day
parseLastModified (Just (_, modified)) = parseTime defaultTimeLocale "%a, %d %b %Y %T %Z" (B.unpack modified)
parseLastModified Nothing = Nothing
