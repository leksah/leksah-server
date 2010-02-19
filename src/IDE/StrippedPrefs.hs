-----------------------------------------------------------------------------
--
-- Module      :  IDE.StrippedPrefs
-- Copyright   :  2007-2009 Juergen Nicklisch-Franken, Hamish Mackenzie
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
) where

import Text.PrinterParser
import Graphics.UI.Editor.Parameters
    (emptyParams, Parameter(..), (<<<-), paraName)
import qualified Text.PrettyPrint as  PP (text)

--
-- | Preferences is a data structure to hold configuration data
--
data Prefs = Prefs {
        sourceDirectories   ::   [FilePath]
    ,   unpackDirectory     ::   Maybe FilePath
    ,   retreiveURL         ::   Maybe String
} deriving(Eq,Show)

defaultPrefs = Prefs {
        sourceDirectories   =   []
    ,   unpackDirectory     =   Nothing
    ,   retreiveURL         =   Just "http://www.leksah.org/packages"
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
    ]
