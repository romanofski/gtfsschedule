{-# LANGUAGE OverloadedStrings #-}
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
{- | This module holds configuration types and functions to read from a configuration file. -}
module CLI.Config where

import           GTFS.Schedule                        (Stop (..), defaultScheduleItemTemplate)

import           Data.Ini                             (Ini, lookupValue,
                                                       parseIni, readIniFile)
import           Data.List                            (elemIndex)
import           Control.Applicative                  (optional, some, (<|>))
import           Control.Monad.Reader                 (ask, asks, local)

import           Options.Applicative.Builder          (argument, auto, flag,
                                                       help, info, long,
                                                       metavar, short, str)
import qualified Options.Applicative.Builder          as Builder
import           Options.Applicative.Builder.Internal (HasValue, Mod)
import           Options.Applicative.Types            (Parser, ReadM (..))

import qualified Data.Text                            as T
import           System.Directory                     (doesFileExist)
import           System.Environment.XDG.BaseDir       (getUserConfigFile)


-- | Command line options
data Command
    = Monitor { stops                :: [Stop]
              , realtime             :: Bool
              , autoUpdate           :: Bool
              , limit                :: Maybe Integer
              , staticDatasetURL     :: Maybe T.Text
              , realtimeFeedURL      :: Maybe T.Text
              , scheduleItemTemplate :: Maybe T.Text}
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


-- | optparse-applicative reader for stop/walktime pair
--
stopWithWalktime :: ReadM Stop
stopWithWalktime = ReadM $ do
  s <- ask
  let
    i = elemIndex '+' s
    sCode = maybe s (`take` s) i
    walktime = maybe "" ((`drop` s) . (+1)) i
  Stop
    <$> local (const sCode) (unReadM str)
    <*> (local (const walktime) (unReadM auto) <|> pure 0)
    <*> pure ""


optionParser ::
  Ini -> Parser Command
optionParser conf =
    Builder.subparser
        (Builder.command
             "monitor"
             (info
                  (monitorOptions conf)
                  (Builder.progDesc
                       "Monitor a stop/station and print next departing services")) <>
         Builder.command
             "setup"
             (info
                  (setupOptions conf)
                  (Builder.progDesc "Setup the gtfsschedule database")) <>
        Builder.command
        "search"
        (info searchOptions (Builder.progDesc "Search for a stop/station ID")))

searchOptions :: Parser Command
searchOptions = Search <$> Builder.argument Builder.str ( metavar "NAME" <> help "Stop name to search the id for")

setupOptions ::
  Ini -> Parser Command
setupOptions conf = Setup
  <$> optional (Builder.option txtReader ( long "static-url" <> withConfigfile conf (T.pack "static-url") <> metavar "URL" <> short 's' <> help "URL to the static dataset zip archive" ))

txtReader :: ReadM T.Text
txtReader = ReadM $ do
  asks T.pack

monitorOptions :: Ini -> Parser Command
monitorOptions conf =
    Monitor <$>
    some
        (argument
             stopWithWalktime
             (metavar "STATION-ID[:MINUTES]" <>
              help
                  "Station id to show the schedule for, and optional walktime in minutes to that station (default: 0))")) <*>
    flag
        False
        True
        (long "realtime" <> short 'r' <>
         help "Enable realtime updates - DEPRECATED") <*>
    flag
        False
        True
        (long "autoupdate" <> short 'u' <>
         help "Automatically update the static GTFS dataset") <*>
    optional (
     Builder.option
         Builder.auto
         (short 'l' <> long "limit" <> help "How many schedule items to show")) <*>
    optional (
     Builder.option
         txtReader
         (long "static-url" <> withConfigfile conf "static-url" <>
          metavar "URL" <>
          help "URL to the static dataset zip archive")) <*>
    optional (
     Builder.option
         txtReader
         (long "realtime-url" <> withConfigfile conf "realtime-url" <>
          metavar "URL" <>
          help "URL to the realtime GTFS feed")) <*>
    optional (
     Builder.option
         txtReader
         (long "schedule-item-template" <>
          withConfigfile conf "schedule-item-template" <>
          help
              ("Template to format a schedule item. Default: " ++
               T.unpack defaultScheduleItemTemplate)))
