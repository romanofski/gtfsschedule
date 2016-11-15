{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{- |  Update functionality: We determine if the static dataset is outdated.
-}
module Update
       (isDatasetUpToDate, printWarningForNewDataset, isCurrent,
        Error(..))
       where

import Network.HTTP.Conduit
import Network.HTTP.Types.Header (ResponseHeaders, Header, hLastModified)
import Data.Time.Calendar (Day)
import System.Locale (defaultTimeLocale)
import Data.Time.Format (parseTime)
import Data.List (find)
import System.IO (hPrint, stderr)
import qualified Data.ByteString.Char8 as B


data Error = Error String
  deriving (Eq, Show)

-- | Returns True if the static dataset has been updated on the remote server.
-- The function performs a HEAD request and extracts the last-modified header. The
-- date is checked against what the database is giving us.
-- Note: uses last modified header to determine if it has recently been updated
--
isDatasetUpToDate ::
  String  -- ^ URL to perform a HEAD against (typically the static dataset zip file)
  -> Day  -- ^ modified date from the database
  -> (Day -> Day -> Bool)
  -> IO (Either Error Bool)
isDatasetUpToDate url dbmodified f = do
    headers <- getHeadersForDataset url
    case getLastModified headers of
        Nothing ->
            return $
            Left
                (Error
                     ("Couldn't find last-modified headers in: " ++
                      show headers))
        Just (_,modifiedHeader) ->
            case parseTime
                     defaultTimeLocale
                     "%a, %d %b %Y %T %Z"
                     (B.unpack modifiedHeader) of
                Just datasetModified ->
                    return $ Right (f datasetModified dbmodified)
                Nothing ->
                    return $
                    Left
                        (Error
                             ("Couldn't parse last modified header: " ++
                              show modifiedHeader))

-- | Prints an additional line to let the user know an updated static dataset is available
printWarningForNewDataset ::
  Either Error Bool
  -> IO ()
printWarningForNewDataset (Right False) = print "Note: New dataset available!"
printWarningForNewDataset (Right _) = return ()
printWarningForNewDataset (Left _) = hPrint stderr "Warning: Couldn't determine if dataset is outdated."

-- | Returns True if the last-modified from the server is less or equal than what we have in our database
isCurrent ::
  Day  -- ^ last-modified parsed from header
  -> Day  -- ^ last-modified date from database
  -> Bool
isCurrent lastModified dbmod = lastModified <= dbmod

--
-- private helpers

getHeadersForDataset ::
  String
  -> IO ResponseHeaders
getHeadersForDataset url = do
  initReq <- parseUrl url
  let request = initReq { method = "HEAD" }
  manager <- newManager conduitManagerSettings
  response <- httpLbs request manager
  return $ responseHeaders response

getLastModified ::
  ResponseHeaders
  -> Maybe Header
getLastModified = find (\(n, _) -> n == hLastModified)
