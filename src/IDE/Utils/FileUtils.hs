{-# OPTIONS_GHC -XScopedTypeVariables -XBangPatterns #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Utils.FileUtils
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

module IDE.Utils.FileUtils (
    allModules
,   allHiFiles
,   allHaskellSourceFiles
,   cabalFileName
,   allCabalFiles
,   getConfigFilePathForLoad
,   hasConfigDir
,   getConfigDir
,   getConfigFilePathForSave
,   getCollectorPath
,   getSysLibDir
,   getHaddockVersion
,   getGhcVersion
,   moduleNameFromFilePath
,   moduleNameFromFilePath'
,   findKnownPackages
,   isSubPath
,   findSourceFile
,   findSourceFile'
,   haskellSrcExts
,   getCabalUserPackageDir
,   autoExtractCabalTarFiles
,   autoExtractTarFiles
,   getInstalledPackageIds
,   figureOutGhcOpts
,   figureOutHaddockOpts
) where

import System.FilePath
       (splitFileName, dropExtension, takeExtension,
        combine, addExtension, (</>), normalise, splitPath)
import Distribution.ModuleName (toFilePath, ModuleName)
import Control.Monad (foldM, filterM)
import Data.Maybe (catMaybes)
import qualified Data.List as  List (init, elem)
import Distribution.Simple.PreProcess.Unlit (unlit)
import System.Directory
    (doesDirectoryExist,
     doesFileExist,
     setCurrentDirectory,
     getCurrentDirectory,
     getDirectoryContents,
     createDirectory,
     getHomeDirectory)
import Text.ParserCombinators.Parsec.Language (haskellDef, haskell)
#if MIN_VERSION_parsec(3,0,0)
import qualified Text.ParserCombinators.Parsec.Token as P
       (GenTokenParser(..), TokenParser, identStart)
#else
import qualified Text.ParserCombinators.Parsec.Token as P
       (TokenParser(..), identStart)
#endif
import Text.ParserCombinators.Parsec
       (GenParser, parse, oneOf, (<|>), alphaNum, noneOf, char, try,
        (<?>), many, CharParser)
import Data.Set (Set)
import Data.List
    (isPrefixOf, isSuffixOf, stripPrefix)
import qualified Data.Set as  Set (empty, fromList)
import Control.Monad.Trans (MonadIO(..))
import Distribution.Package (PackageIdentifier)
import System.Process
    (waitForProcess, runCommand)
import Data.Char (ord)
import Distribution.Text (simpleParse)

import IDE.Utils.Utils
import IDE.Core.CTypes(configDirName)
import qualified Distribution.Text as  T (simpleParse)
import System.Log.Logger(errorM,warningM,debugM)
import IDE.Utils.Tool

haskellSrcExts :: [String]
haskellSrcExts = ["hs","lhs","chs","hs.pp","lhs.pp","chs.pp","hsc"]

-- | Returns True if the second path is a location which starts with the first path
isSubPath :: FilePath -> FilePath -> Bool
isSubPath fp1 fp2 =
    let fpn1    =   splitPath $ normalise fp1
        fpn2    =   splitPath $ normalise fp2
        res     =   isPrefixOf fpn1 fpn2
    in res

findSourceFile :: [FilePath]
    -> [String]
    -> ModuleName
    -> IO (Maybe FilePath)
findSourceFile directories exts modId  =
    let modulePath      =   toFilePath modId
        allPathes       =   map (\ d -> d </> modulePath) directories
        allPossibles    =   concatMap (\ p -> map (addExtension p) exts)
                                allPathes
    in  find' allPossibles

findSourceFile' :: [FilePath]
    -> String
    -> IO (Maybe FilePath)
findSourceFile' directories modulePath  =
    let allPathes       =   map (\ d -> d </> modulePath) directories
    in  find' allPathes


find' :: [FilePath] -> IO (Maybe FilePath)
find' []            =   return Nothing
find' (h:t)         =   catch (do
    exists <- doesFileExist h
    if exists
        then return (Just h)
        else find' t)
        $ \ _ -> return Nothing

-- | The directory where config files reside
--
getConfigDir :: IO FilePath
getConfigDir = do
    d <- getHomeDirectory
    let filePath = d </> configDirName
    exists <- doesDirectoryExist filePath
    if exists
        then return filePath
        else do
            createDirectory filePath
            return filePath

getConfigDirForLoad :: IO (Maybe FilePath)
getConfigDirForLoad = do
    d <- getHomeDirectory
    let filePath = d </> configDirName
    exists <- doesDirectoryExist filePath
    if exists
        then return (Just filePath)
        else return Nothing

hasConfigDir :: IO Bool
hasConfigDir = do
    d <- getHomeDirectory
    let filePath = d </> configDirName
    doesDirectoryExist filePath


getConfigFilePathForLoad :: String -> Maybe FilePath -> FilePath -> IO FilePath
getConfigFilePathForLoad fn mbFilePath dataDir = do
    mbCd <- case mbFilePath of
                Just p -> return (Just p)
                Nothing -> getConfigDirForLoad
    case mbCd of
        Nothing -> getFromData
        Just cd -> do
            ex <- doesFileExist (cd </> fn)
            if ex
                then return (cd </> fn)
                else getFromData
    where getFromData = do
            ex <- doesFileExist (dataDir </> "data" </> fn)
            if ex
                then return (dataDir </> "data" </> fn)
                else error $"Config file not found: " ++ fn

getConfigFilePathForSave :: String -> IO FilePath
getConfigFilePathForSave fn = do
    cd <- getConfigDir
    return (cd </> fn)

allModules :: FilePath -> IO [ModuleName]
allModules filePath = catch (do
    exists <- doesDirectoryExist filePath
    if exists
        then do
            filesAndDirs <- getDirectoryContents filePath
            let filesAndDirs' = map (\s -> combine filePath s)
                                    $filter (\s -> s /= "." && s /= ".." && s /= "_darcs" && s /= "dist"
                                        && s /= "Setup.lhs") filesAndDirs
            dirs <-  filterM (\f -> doesDirectoryExist f) filesAndDirs'
            files <-  filterM (\f -> doesFileExist f) filesAndDirs'
            let hsFiles =   filter (\f -> let ext = takeExtension f in
                                            ext == ".hs" || ext == ".lhs") files
            mbModuleStrs <- mapM moduleNameFromFilePath hsFiles
            let mbModuleNames = catMaybes $
                                    map (\n -> case n of
                                                    Nothing -> Nothing
                                                    Just s -> simpleParse s)
                                        mbModuleStrs
            otherModules <- mapM allModules dirs
            return (mbModuleNames ++ concat otherModules)
        else return [])
            $ \ _ -> return []

allHiFiles :: FilePath -> IO [FilePath]
allHiFiles = allFilesWithExtensions [".hi"] True []

allCabalFiles :: FilePath -> IO [FilePath]
allCabalFiles = allFilesWithExtensions [".cabal"] False []

allHaskellSourceFiles :: FilePath -> IO [FilePath]
allHaskellSourceFiles = allFilesWithExtensions [".hs",".lhs"] True []

allFilesWithExtensions :: [String] -> Bool -> [FilePath] -> FilePath -> IO [FilePath]
allFilesWithExtensions extensions recurseFurther collecting filePath = catch (do
    exists <- doesDirectoryExist filePath
    if exists
        then do
            filesAndDirs <- getDirectoryContents filePath
            let filesAndDirs' = map (\s -> combine filePath s)
                                    $filter (\s -> s /= "." && s /= ".." && s /= "_darcs") filesAndDirs
            dirs    <-  filterM (\f -> doesDirectoryExist f) filesAndDirs'
            files   <-  filterM (\f -> doesFileExist f) filesAndDirs'
            let choosenFiles =   filter (\f -> let ext = takeExtension f in
                                                    List.elem ext extensions) files
            allFiles <-
                if recurseFurther || (not recurseFurther && null choosenFiles)
                    then foldM (allFilesWithExtensions extensions recurseFurther) (choosenFiles ++ collecting) dirs
                    else return (choosenFiles ++ collecting)
            return (allFiles)
        else return collecting)
            $ \ _ -> return collecting


moduleNameFromFilePath :: FilePath -> IO (Maybe String)
moduleNameFromFilePath fp = catch (do
    exists <- doesFileExist fp
    if exists
        then do
            str <-  readFile fp
            moduleNameFromFilePath' fp str
        else return Nothing)
            $ \ _ -> return Nothing

moduleNameFromFilePath' :: FilePath -> String -> IO (Maybe String)
moduleNameFromFilePath' fp str = do
    let unlitRes = if takeExtension fp == ".lhs"
                    then unlit fp str
                    else Left str
    case unlitRes of
        Right err -> do
            errorM "leksah-server" (show err)
            return Nothing
        Left str' -> do
            let parseRes = parse moduleNameParser fp str'
            case parseRes of
                Left _ -> do
                    return Nothing
                Right str'' -> return (Just str'')

lexer :: P.TokenParser st
lexer = haskell

lexeme :: CharParser st a -> CharParser st a
lexeme = P.lexeme lexer

whiteSpace :: CharParser st ()
whiteSpace = P.whiteSpace lexer

symbol :: String -> CharParser st String
symbol = P.symbol lexer

moduleNameParser :: CharParser () String
moduleNameParser = do
    whiteSpace
    many skipPreproc
    whiteSpace
    symbol "module"
    str <- lexeme mident
    return str
    <?> "module identifier"

skipPreproc :: CharParser () ()
skipPreproc = do
    try (do
        whiteSpace
        char '#'
        many (noneOf "\n")
        return ())
    <?> "preproc"

mident :: GenParser Char st String
mident
        = do{ c <- P.identStart haskellDef
            ; cs <- many (alphaNum <|> oneOf "_'.")
            ; return (c:cs)
            }
        <?> "midentifier"

findKnownPackages :: FilePath -> IO (Set String)
findKnownPackages filePath = catch (do
    paths           <-  getDirectoryContents filePath
    let nameList    =   map dropExtension  $filter (\s -> leksahMetadataSystemFileExtension `isSuffixOf` s) paths
    return (Set.fromList nameList))
        $ \ _ -> return (Set.empty)

cabalFileName :: FilePath -> IO (Maybe String)
cabalFileName filePath = catch (do
    exists <- doesDirectoryExist filePath
    if exists
        then do
            filesAndDirs <- getDirectoryContents filePath
            files <-  filterM (\f -> doesFileExist f) filesAndDirs
            let cabalFiles =   filter (\f -> let ext = takeExtension f in ext == ".cabal") files
            if null cabalFiles
                then return Nothing
                else if length cabalFiles == 1
                    then return (Just $head cabalFiles)
                    else do
                        warningM "leksah-server" "Multiple cabal files"
                        return Nothing
        else return Nothing)
        (\_ -> return Nothing)

getCabalUserPackageDir :: IO (Maybe FilePath)
getCabalUserPackageDir = do
    (!output,_) <- runTool' "cabal" ["help"] Nothing
    case stripPrefix "  " (toolline $ last output) of
        Just s | "config" `isSuffixOf` s -> return $ Just $ take (length s - 6) s ++ "packages"
        _ -> return Nothing

autoExtractCabalTarFiles :: FilePath -> IO ()
autoExtractCabalTarFiles filePath = do
    dir <- getCurrentDirectory
    autoExtractTarFiles' filePath
    setCurrentDirectory dir

autoExtractTarFiles :: FilePath -> IO ()
autoExtractTarFiles filePath = do
    dir <- getCurrentDirectory
    autoExtractTarFiles' filePath
    setCurrentDirectory dir

autoExtractTarFiles' :: FilePath -> IO ()
autoExtractTarFiles' filePath =
    catch (do
        exists <- doesDirectoryExist filePath
        if exists
            then do
                filesAndDirs             <- getDirectoryContents filePath
                let filesAndDirs'        =  map (\s -> combine filePath s)
                                                $ filter (\s -> s /= "." && s /= ".." && not (isPrefixOf "00-index" s)) filesAndDirs
                dirs                     <- filterM (\f -> doesDirectoryExist f) filesAndDirs'
                files                    <- filterM (\f -> doesFileExist f) filesAndDirs'
                let choosenFiles         =  filter (\f -> isSuffixOf ".tar.gz" f) files
                let decompressionTargets =  filter (\f -> (dropExtension . dropExtension) f `notElem` dirs) choosenFiles
                mapM_ (\f -> let (dir,fn) = splitFileName f
                                 command = "tar -zxf " ++ fn in do
                                    setCurrentDirectory dir
                                    handle   <- runCommand command
                                    waitForProcess handle
                                    return ())
                        decompressionTargets
                mapM_ autoExtractTarFiles' dirs
                return ()
            else return ()
    ) $ \ _ -> return ()


getCollectorPath :: MonadIO m => m FilePath
getCollectorPath = liftIO $ do
    configDir <- getConfigDir
    let filePath = configDir </> "metadata"
    exists    <- doesDirectoryExist filePath
    if exists
        then return filePath
        else do
            createDirectory filePath
            return filePath

getGhcVersion :: IO FilePath
getGhcVersion = catch (do
    (!output,_) <- runTool' "ghc" ["--numeric-version"] Nothing
    let vers = toolline $ head output
        vers2 = if ord (last vers) == 13
                    then List.init vers
                    else vers
    debugM "leksah-server" $ "Got GHC Version " ++ vers2
    return vers2
    ) $ \ _ -> error ("FileUtils>>getGhcVersion failed")

getHaddockVersion :: IO String
getHaddockVersion = catch (do
    (!output,_) <- runTool' "haddock" ["--version"] Nothing
    let vers = toolline $ head output
        vers2 = if ord (last vers) == 13
                    then List.init vers
                    else vers
    return vers2
    ) $ \ _ -> error ("FileUtils>>getHaddockVersion failed")

getSysLibDir :: IO FilePath
getSysLibDir = catch (do
    (!output,_) <- runTool' "ghc" ["--print-libdir"] Nothing
    let libDir = toolline $ head output
        libDir2 = if ord (last libDir) == 13
                    then List.init libDir
                    else libDir
    return (normalise libDir2)
    ) $ \ _ -> error ("FileUtils>>getSysLibDir failed")

getInstalledPackageIds :: IO [PackageIdentifier]
getInstalledPackageIds = catch (do
    (!output, _) <- runTool' "ghc-pkg" ["list", "--simple-output"] Nothing
    let names = toolline $ head output
    return (catMaybes (map T.simpleParse (words names)))
    ) $ \ _ -> error ("FileUtils>>getInstalledPackageIds failed")

figureOutHaddockOpts :: IO [String]
figureOutHaddockOpts = do
    (!output,_) <- runTool' "cabal" (["haddock","--with-haddock=leksahecho","--executables"]) Nothing
    let opts = concatMap (words . toolline) output
    return (filterOptGhc opts)
    where
        filterOptGhc []    = []
        filterOptGhc (s:r) = case stripPrefix "--optghc=" s of
                                    Nothing -> filterOptGhc r
                                    Just s'  -> s' : filterOptGhc r

figureOutGhcOpts :: IO [String]
figureOutGhcOpts = do
    debugM "leksah-server" "figureOutGhcOpts"
    (!output,_) <- runTool' "runhaskell" ["Setup","build","--with-ghc=leksahecho"] Nothing
    case catMaybes $ map (findMake . toolline) output of
        options:_ -> return (words options)
        _         -> return []
    where
        findMake [] = Nothing
        findMake line@(_:xs) =
                case stripPrefix " --make " line of
                    Nothing -> findMake xs
                    s -> s
