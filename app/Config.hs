{-# LANGUAGE OverloadedStrings #-}
{- | This module holds configuration types and functions to read from a configuration file. -}
module Config
       (withConfigfile, loadGTFSConfig, Command(..))
       where

import GTFS.Schedule (StopWithWalktime)

import Data.Ini (readIniFile, parseIni, lookupValue, Ini)

import Options.Applicative.Builder.Internal (HasValue, Mod)
import qualified Options.Applicative.Builder as Builder

import System.Environment.XDG.BaseDir (getUserConfigFile)
import System.Directory (doesFileExist)
import qualified Data.Text as T


-- | Command line options
data Command
    = Monitor { stopsWithWalktime :: [StopWithWalktime]
              , realtime :: Bool
              , autoUpdate :: Bool
              , limit :: Maybe Integer
              , staticDatasetURL :: Maybe T.Text
              , realtimeFeedURL :: Maybe T.Text}
    | Setup { staticDatasetURL :: Maybe T.Text }
    | Search { searchString :: String }
    deriving (Show)

loadGTFSConfig :: IO Ini
loadGTFSConfig = do
    configpath <- gtfsConfigFilePath
    configExists <- doesFileExist configpath
    if configExists
        then do
            result <- readIniFile configpath
            case result of
                Left err -> error err
                Right conf -> return conf
        else case parseIni "" of
                 Left err -> error err
                 Right conf -> return conf

gtfsConfigFilePath :: IO FilePath
gtfsConfigFilePath = getUserConfigFile "gtfs" "config.cfg"

withConfigfile :: (HasValue f) => Ini -> T.Text -> Mod f T.Text
withConfigfile conf k = either (const Builder.idm) Builder.value  $ lookupValue "default" k conf

