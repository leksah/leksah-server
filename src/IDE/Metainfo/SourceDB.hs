
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Metainfo.SourceDB
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

module IDE.Metainfo.SourceDB (

    buildSourceForPackageDB
,   sourceForPackage
,   parseSourceForPackageDB
,   getSourcesMap

) where

import IDE.StrippedPrefs (Prefs(..))
import Data.Map (Map(..))
import Distribution.Package (PackageIdentifier(..))
import IDE.Utils.Utils (standardSourcesFilename)
import qualified Data.Map as Map
       (fromList, toList, fromListWith, lookup)
import IDE.Utils.FileUtils
       (getConfigFilePathForLoad, getConfigFilePathForSave, allCabalFiles)
import System.Directory (doesFileExist, canonicalizePath)
import Data.List (foldl')
import qualified Text.PrettyPrint as PP
       (colon, (<>), text, ($$), vcat, Doc(..), render, char)
import Text.ParserCombinators.Parsec
       (try, char, unexpected, noneOf, eof, many, CharParser(..),
        parseFromFile, (<?>), (<|>))
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as P
       (symbol, whiteSpace, makeTokenParser, commentLine, commentEnd,
        commentStart,LanguageDef)
import Data.Maybe (catMaybes)
import IDE.Core.CTypes (packageIdentifierFromString)
import Paths_leksah_server
import System.Log.Logger(errorM,debugM)

-- ---------------------------------------------------------------------
-- Function to map packages to file paths
--

getSourcesMap :: Prefs -> IO (Map PackageIdentifier [FilePath])
getSourcesMap prefs = do
        mbSources <- parseSourceForPackageDB
        case mbSources of
            Just map -> return map
            Nothing -> do
                buildSourceForPackageDB prefs
                mbSources <- parseSourceForPackageDB
                case mbSources of
                    Just map -> do
                        return map
                    Nothing ->  error "can't build/open source for package file"

sourceForPackage :: PackageIdentifier
    -> (Map PackageIdentifier [FilePath])
    -> Maybe FilePath
sourceForPackage id map =
    case id `Map.lookup` map of
        Just (h:_)  ->  Just h
        _           ->  Nothing

buildSourceForPackageDB :: Prefs -> IO ()
buildSourceForPackageDB prefs = do
    let sourceDirs  =   sourceDirectories prefs
    let dirs        =   case unpackDirectory prefs of
                            Just dir -> dir : sourceDirs
                            Nothing ->  sourceDirs
    cabalFiles      <-  mapM allCabalFiles dirs
    fCabalFiles     <-  mapM canonicalizePath $ concat cabalFiles
    mbPackages      <-  mapM (\fp -> parseCabal fp) fCabalFiles
    let pdToFiles   =   Map.fromListWith (++)
                $ map (\(Just p,o ) -> (p,o))
                    $ filter (\(mb, _) -> case mb of
                                            Nothing -> False
                                            _       -> True )
                        $ zip mbPackages (map (\a -> [a]) fCabalFiles)
    filePath        <-  getConfigFilePathForSave standardSourcesFilename
    writeFile filePath  (PP.render (showSourceForPackageDB pdToFiles))

showSourceForPackageDB  :: Map String [FilePath] -> PP.Doc
showSourceForPackageDB aMap = PP.vcat (map showIt (Map.toList aMap))
    where
    showIt :: (String,[FilePath]) -> PP.Doc
    showIt (pd,list) =  (foldl' (\l n -> l PP.$$ (PP.text $ show n)) label list)
                             PP.<>  PP.char '\n'
        where label  =  PP.text pd PP.<> PP.colon

parseSourceForPackageDB :: IO (Maybe (Map PackageIdentifier [FilePath]))
parseSourceForPackageDB = do
    dataDir         <-  getDataDir
    filePath        <-  getConfigFilePathForLoad standardSourcesFilename Nothing dataDir
    exists          <-  doesFileExist filePath
    if exists
        then do
            res             <-  parseFromFile sourceForPackageParser filePath
            case res of
                Left pe ->  do
                    errorM "leksah-server" $ "Error reading source packages file "
                            ++ filePath ++ " " ++ show pe
                    return Nothing
                Right r ->  return (Just r)
        else do
            errorM "leksah-server" $" No source packages file found: " ++ filePath
            return Nothing

--
-- ---------------------------------------------------------------------
-- | Parser for Package DB
--
packageStyle  :: P.LanguageDef st
packageStyle  = emptyDef
                { P.commentStart   = "{-"
                , P.commentLine    = "--"
                , P.commentEnd     = "-}"
                }

lexer       =   P.makeTokenParser packageStyle
whiteSpace  =   P.whiteSpace lexer
symbol      =   P.symbol lexer

sourceForPackageParser :: CharParser () (Map PackageIdentifier [FilePath])
sourceForPackageParser = do
    whiteSpace
    ls  <-  many onePackageParser
    whiteSpace
    eof
    return (Map.fromList (catMaybes ls))
    <?> "sourceForPackageParser"

onePackageParser :: CharParser () (Maybe (PackageIdentifier,[FilePath]))
onePackageParser = do
    mbPd        <-  packageDescriptionParser
    filePaths   <-  many filePathParser
    case mbPd of
        Nothing -> return Nothing
        Just pd -> return (Just (pd,filePaths))
    <?> "onePackageParser"

packageDescriptionParser :: CharParser () (Maybe PackageIdentifier)
packageDescriptionParser = try (do
    whiteSpace
    str <- many (noneOf ":")
    char ':'
    return (packageIdentifierFromString str))
    <?> "packageDescriptionParser"

filePathParser :: CharParser () FilePath
filePathParser = try (do
    whiteSpace
    char '"'
    str <- many (noneOf ['"'])
    char '"'
    return (str))
    <?> "filePathParser"

parseCabal :: FilePath -> IO (Maybe String)
parseCabal fn = do
    --putStrLn $ "Now parsing minimal " ++ fn
    res     <-  parseFromFile cabalMinimalParser fn
    case res of
        Left pe ->  do
            errorM "leksah-server" $"Error reading cabal file " ++ show fn ++ " " ++ show pe
            return Nothing
        Right r ->  do
            debugM "leksah-server" r
            return (Just r)

cabalMinimalParser :: CharParser () String
cabalMinimalParser = do
    r1 <- cabalMinimalP
    r2 <- cabalMinimalP
    case r1 of
        Left v -> do
            case r2 of
                Right n -> return (n ++ "-" ++ v)
                Left _ -> unexpected "Illegal cabal"
        Right n -> do
            case r2 of
                Left v -> return (n ++ "-" ++ v)
                Right _ -> unexpected "Illegal cabal"

cabalMinimalP :: CharParser () (Either String String)
cabalMinimalP =
    do  try $(symbol "name:" <|> symbol "Name:")
        whiteSpace
        name       <-  (many $noneOf " \n")
        (many $noneOf "\n")
        char '\n'
        return (Right name)
    <|> do
            try $(symbol "version:" <|> symbol "Version:")
            whiteSpace
            version    <-  (many $noneOf " \n")
            (many $noneOf "\n")
            char '\n'
            return (Left version)
    <|> do
            many $noneOf "\n"
            char '\n'
            cabalMinimalP
    <?> "cabal minimal"



