{-# LANGUAGE OverloadedStrings #-}

{- | This module holds configuration types and functions to read from a configuration file. -}
module CLI.Config where

import GTFS.Schedule (Stop(..), defaultScheduleItemTemplate)

import Data.Ini (readIniFile, parseIni, lookupValue, Ini)

import Control.Applicative ((<$>), (<*>), (<|>), pure, some, optional)
import Data.List (findIndex)
import Data.Monoid ((<>))

import Control.Monad.Reader (ask, local)

import Options.Applicative.Builder.Internal (HasValue, Mod)
import qualified Options.Applicative.Builder as Builder
import Options.Applicative.Builder (
  long, help, argument, metavar, flag, str, info, short, auto)
import Options.Applicative.Types (Parser, ReadM(..))

import System.Environment.XDG.BaseDir (getUserConfigFile)
import System.Directory (doesFileExist)
import qualified Data.Text as T


-- | Command line options
data Command
    = Monitor { stops :: [Stop]
              , realtime :: Bool
              , autoUpdate :: Bool
              , limit :: Maybe Integer
              , staticDatasetURL :: Maybe T.Text
              , realtimeFeedURL :: Maybe T.Text
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
    i = findIndex (== '+') s
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
        (info (searchOptions) (Builder.progDesc "Search for a stop/station ID")))

searchOptions :: Parser Command
searchOptions = Search <$> Builder.argument Builder.str ( metavar "NAME" <> help "Stop name to search the id for")

setupOptions ::
  Ini -> Parser Command
setupOptions conf = Setup
  <$> (optional $ Builder.option txtReader ( long "static-url" <> withConfigfile conf (T.pack "static-url") <> metavar "URL" <> short 's' <> help "URL to the static dataset zip archive" ))

txtReader :: ReadM T.Text
txtReader = ReadM $ do
  s <- ask
  return $ T.pack s

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
    (optional $
     Builder.option
         Builder.auto
         (short 'l' <> long "limit" <> help "How many schedule items to show")) <*>
    (optional $
     Builder.option
         txtReader
         (long "static-url" <> withConfigfile conf "static-url" <>
          metavar "URL" <>
          help "URL to the static dataset zip archive")) <*>
    (optional $
     Builder.option
         txtReader
         (long "realtime-url" <> withConfigfile conf "realtime-url" <>
          metavar "URL" <>
          help "URL to the realtime GTFS feed")) <*>
    (optional $
     Builder.option
         txtReader
         (long "schedule-item-template" <>
          withConfigfile conf "schedule-item-template" <>
          help
              ("Template to format a schedule item. Default: " ++
               T.unpack defaultScheduleItemTemplate)))
