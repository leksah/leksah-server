-----------------------------------------------------------------------------
--
-- Module      :  IDE.StrippedPrefs
-- Copyright   :  2007-2010 Juergen Nicklisch-Franken, Hamish Mackenzie
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
,   readStrippedPrefs
,   writeStrippedPrefs
,   getSourceDirectories
,   getUnpackDirectory
) where

import Text.PrinterParser
import Graphics.UI.Editor.Parameters
    (emptyParams, Parameter(..), (<<<-), paraName)
import qualified Text.PrettyPrint as  PP (text)
import System.FilePath
       (joinPath, (</>), dropTrailingPathSeparator, splitPath)
import System.Directory (getHomeDirectory)
import Control.Monad (liftM)

--
-- | Preferences is a data structure to hold configuration data
--
data Prefs = Prefs {
        sourceDirectories   ::   [FilePath]
    ,   unpackDirectory     ::   Maybe FilePath
    ,   retreiveURL         ::   Maybe String
    ,   serverPort          ::   Int
} deriving(Eq,Show)

defaultPrefs :: Prefs
defaultPrefs = Prefs {
        sourceDirectories   =   []
    ,   unpackDirectory     =   Nothing
    ,   retreiveURL         =   Just "http://www.leksah.org/"
    ,   serverPort          =   11111
    }

-- ------------------------------------------------------------
-- * Parsing
-- ------------------------------------------------------------

readStrippedPrefs :: FilePath -> IO Prefs
readStrippedPrefs fn = readFields fn prefsDescription defaultPrefs

writeStrippedPrefs :: FilePath -> Prefs -> IO ()
writeStrippedPrefs fpath prefs = writeFields fpath prefs prefsDescription


prefsDescription :: [FieldDescriptionS Prefs]
prefsDescription = [
        mkFieldS
            (paraName <<<- ParaName
                "Paths under which haskell sources for packages may be found" $ emptyParams)
            (PP.text . show)
            readParser
            sourceDirectories
            (\b a -> a{sourceDirectories = b})
    ,   mkFieldS
            (paraName <<<- ParaName "Maybe a directory for unpacking cabal packages" $ emptyParams)
            (PP.text . show)
            readParser
            unpackDirectory
            (\b a -> a{unpackDirectory = b})
    ,   mkFieldS
            (paraName <<<- ParaName "Maybe an URL to load prebuild metadata " $ emptyParams)
            (PP.text . show)
            readParser
            retreiveURL
            (\b a -> a{retreiveURL = b})
    ,   mkFieldS
            (paraName <<<- ParaName "Port number for server connection" $ emptyParams)
            (PP.text . show)
            intParser
            serverPort
            (\b a -> a{serverPort = b})
    ]

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
getSourceDirectories = (mapM expandHomePath) . sourceDirectories

getUnpackDirectory :: Prefs -> IO (Maybe FilePath)
getUnpackDirectory = maybe (return Nothing) (liftM Just . expandHomePath) . unpackDirectory
