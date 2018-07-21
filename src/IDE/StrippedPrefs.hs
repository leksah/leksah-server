{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.StrippedPrefs
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.StrippedPrefs (

    Prefs(..)
,   RetrieveStrategy(..)
,   readStrippedPrefs
,   writeStrippedPrefs
,   getSourceDirectories
,   getUnpackDirectory
) where

import GHC.Generics (Generic)
import qualified Text.PrettyPrint as  PP (text)
import System.FilePath
       (joinPath, (</>), dropTrailingPathSeparator, splitPath)
import System.Directory (getHomeDirectory)
import Control.Monad (liftM)
import IDE.Core.CTypes (RetrieveStrategy(..), configDirName)
import Data.Text (Text)
import qualified Data.Aeson as JSON (Result(..), Value)
import Data.Aeson
       (eitherDecode, fromJSON, toJSON, FromJSON(..), ToJSON(..))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types
       (Options, genericParseJSON, genericToEncoding, genericToJSON,
        defaultOptions, fieldLabelModifier)
import Data.Monoid ((<>))
import qualified Control.Exception as E (catch)
import Control.Exception (IOException)
import qualified Data.Map as M (lookup)
import Data.Map (Map)
import qualified Data.ByteString.Lazy as LBS (writeFile, readFile)
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)

--
-- | Preferences is a data structure to hold configuration data
--
data Prefs = Prefs
  { sourceDirectories :: [FilePath]
  , unpackDirectory   :: Maybe FilePath
  , retrieveURL       :: Text
  , retrieveStrategy  :: RetrieveStrategy
  , serverPort        :: Int
  , endWithLastConn   :: Bool
  } deriving(Eq, Show, Generic)

data PrefsFile = PrefsFile
  { sourceDirectories_ :: Maybe [FilePath]
  , unpackDirectory_   :: Maybe (Maybe FilePath)
  , retrieveURL_       :: Maybe Text
  , retrieveStrategy_  :: Maybe RetrieveStrategy
  , serverPort_        :: Maybe Int
  , endWithLastConn_   :: Maybe Bool
  } deriving(Eq, Show, Generic)

prefsAesonOptions :: Options
prefsAesonOptions = defaultOptions
    { fieldLabelModifier = init
    }

instance ToJSON PrefsFile where
    toJSON     = genericToJSON prefsAesonOptions
    toEncoding = genericToEncoding prefsAesonOptions
instance FromJSON PrefsFile where
    parseJSON = genericParseJSON prefsAesonOptions

defaultPrefs :: Prefs
defaultPrefs = Prefs
  { sourceDirectories = []
  , unpackDirectory   = Just ("~" </> configDirName </> "packageSources")
  , retrieveURL       = "http://leksah.github.io/"
  , retrieveStrategy  = RetrieveThenBuild
  , serverPort        = 11111
  , endWithLastConn   = True
  }

mergePrefsFile :: Prefs -> PrefsFile -> Prefs
mergePrefsFile Prefs{..} PrefsFile{..} = Prefs
  { sourceDirectories = fromMaybe sourceDirectories sourceDirectories_
  , unpackDirectory   = fromMaybe unpackDirectory unpackDirectory_
  , retrieveURL       = fromMaybe retrieveURL retrieveURL_
  , retrieveStrategy  = fromMaybe retrieveStrategy retrieveStrategy_
  , serverPort        = fromMaybe serverPort serverPort_
  , endWithLastConn   = fromMaybe endWithLastConn endWithLastConn_
  }

toPrefsFile :: Prefs -> PrefsFile
toPrefsFile Prefs{..} = PrefsFile
  { sourceDirectories_ = Just sourceDirectories
  , unpackDirectory_   = Just unpackDirectory
  , retrieveURL_       = Just retrieveURL
  , retrieveStrategy_  = Just retrieveStrategy
  , serverPort_        = Just serverPort
  , endWithLastConn_   = Just endWithLastConn
  }



-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

readStrippedPrefs :: HasCallStack => FilePath -> IO Prefs
readStrippedPrefs file = E.catch (
    eitherDecode <$> LBS.readFile file >>= \case
        Left e -> error $ "Error reading file " ++ show file ++ " " ++ show e
        Right r -> return $ mergePrefsFile defaultPrefs r)
    (\ (e::IOException) -> error $ "Error reading file " ++ show file ++ " " ++ show e)

writeStrippedPrefs :: FilePath -> Prefs -> IO ()
writeStrippedPrefs file prefs = LBS.writeFile file . encodePretty $ toPrefsFile prefs


-- ------------------------------------------------------------
-- * Cross platform support for "~" at the start of paths
-- ------------------------------------------------------------

-- | Expand the users home folder into paths such as "~/x"
expandHomePath :: FilePath -> IO FilePath
expandHomePath p = case splitPath p of
    h : rest | dropTrailingPathSeparator h == "~" ->  do
        home <- getHomeDirectory
        return $ home </> joinPath rest
    _ -> return p

getSourceDirectories :: Prefs -> IO [FilePath]
getSourceDirectories = mapM expandHomePath . sourceDirectories

getUnpackDirectory :: Prefs -> IO (Maybe FilePath)
getUnpackDirectory = maybe (return Nothing) (liftM Just . expandHomePath) . unpackDirectory
