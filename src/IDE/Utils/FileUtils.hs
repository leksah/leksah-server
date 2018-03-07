{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Utils.FileUtils
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

module IDE.Utils.FileUtils (
    allModules
,   allHiFiles
,   allHaskellSourceFiles
,   isEmptyDirectory
,   cabalFileName
,   saveNixCache
,   loadNixEnv
,   runProjectTool
,   allCabalFiles
,   getConfigFilePathForLoad
,   hasSavedConfigFile
,   getConfigDir
,   getConfigFilePathForSave
,   getCollectorPath
,   getSysLibDir
,   moduleNameFromFilePath
,   moduleNameFromFilePath'
,   moduleCollectorFileName
,   findKnownPackages
,   isSubPath
,   findSourceFile
,   findSourceFile'
,   haskellSrcExts
,   getCabalUserPackageDir
,   autoExtractCabalTarFiles
,   autoExtractTarFiles
,   getInstalledPackages
,   findProjectRoot
,   cabalProjectBuildDir
,   getPackageDBs'
,   getPackageDBs
,   figureOutGhcOpts
,   figureOutGhcOpts'
,   figureOutHaddockOpts
,   allFilesWithExtensions
,   myCanonicalizePath
) where

import Control.Applicative
import Control.Arrow (second)
import Prelude hiding (readFile)
import System.FilePath
       (splitFileName, dropExtension, takeExtension,
        combine, addExtension, (</>), normalise, splitPath, takeFileName,
        takeDirectory, dropFileName, takeBaseName)
import Distribution.ModuleName (toFilePath, ModuleName)
import Control.Monad (when, foldM, filterM, forM, join)
import Data.Maybe (fromMaybe, mapMaybe, catMaybes, listToMaybe)
import Distribution.Simple.PreProcess.Unlit (unlit)
import System.Directory
       (createDirectoryIfMissing, getAppUserDataDirectory,
        canonicalizePath, doesDirectoryExist, doesFileExist,
        setCurrentDirectory, getCurrentDirectory, getDirectoryContents,
        getHomeDirectory)
import Text.ParserCombinators.Parsec.Language (haskellDef, haskell)
import qualified Text.ParserCombinators.Parsec.Token as P
       (GenTokenParser(..), TokenParser, identStart)
import Text.ParserCombinators.Parsec
       (GenParser, parse, oneOf, alphaNum, noneOf, char, try,
        (<?>), CharParser)
import Data.Set (Set)
import Data.List
       (isPrefixOf, isSuffixOf, stripPrefix, nub)
import qualified Data.Set as  Set (empty, fromList)
import Distribution.Package (UnitId)
import Data.Char (ord, isAlphaNum)
import Distribution.Text (simpleParse, display)

import IDE.Utils.Utils
import IDE.Core.CTypes(configDirName, ModuleKey(..))
import qualified Distribution.Text as  T (simpleParse)
import System.Log.Logger(errorM,warningM,debugM)
import IDE.Utils.Tool
import Control.Monad.IO.Class (MonadIO(..), MonadIO)
import Control.Exception as E (SomeException, catch)
import System.IO.Strict (readFile)
import qualified Data.Text as T
       (pack, stripPrefix, isSuffixOf, take, length, unpack,
        words, splitOn, dropWhileEnd, stripSuffix)
import Data.Monoid ((<>))
import Data.Text (Text)
import Control.DeepSeq (deepseq)
import IDE.Utils.VersionUtils (ghcExeName, getDefaultGhcVersion)
import Data.Aeson (eitherDecodeStrict')
import IDE.Utils.CabalPlan (PlanJson(..))
import IDE.Utils.CabalProject (findProjectRoot)
import qualified Data.ByteString as BS (readFile)
import System.Exit (ExitCode(ExitSuccess))
import Data.Map (Map)
import qualified Data.Text.IO as T (writeFile, readFile)
import Text.Read.Compat (readMaybe)
import qualified Data.Map as M (insert, fromList, toList, lookup)
import System.Process (showCommandForUser)

haskellSrcExts :: [FilePath]
haskellSrcExts = ["hs","lhs","chs","hs.pp","lhs.pp","chs.pp","hsc"]

-- | canonicalizePath without crashing
myCanonicalizePath :: FilePath -> IO FilePath
myCanonicalizePath fp = do
    exists <- doesFileExist fp
    if exists
        then canonicalizePath fp
        else return fp


-- | Returns True if the second path is a location which starts with the first path
isSubPath :: FilePath -> FilePath -> Bool
isSubPath fp1 fp2 =
    let fpn1    =   splitPath $ normalise fp1
        fpn2    =   splitPath $ normalise fp2
        res     =   isPrefixOf fpn1 fpn2
    in res

findSourceFile :: [FilePath]
    -> [FilePath]
    -> ModuleName
    -> IO (Maybe FilePath)
findSourceFile directories exts modId  =
    let modulePath      =   toFilePath modId
        allPathes       =   map (</> modulePath) directories
        allPossibles    =   concatMap (\ p -> map (addExtension p) exts)
                                allPathes
    in  find' allPossibles

findSourceFile' :: [FilePath]
    -> FilePath
    -> IO (Maybe FilePath)
findSourceFile' directories modulePath  =
    let allPathes       =   map (</> modulePath) directories
    in  find' allPathes


find' :: [FilePath] -> IO (Maybe FilePath)
find' []            =   return Nothing
find' (h:t)         =   E.catch (do
    exists <- doesFileExist h
    if exists
        then Just <$> canonicalizePath h
        else find' t)
        $ \ (_ :: SomeException) -> return Nothing

-- | The directory where config files reside
--
getConfigDir :: IO FilePath
getConfigDir = do
    d <- getHomeDirectory
    let filePath = d </> configDirName
    createDirectoryIfMissing False filePath
    return filePath

getConfigDirForLoad :: IO (Maybe FilePath)
getConfigDirForLoad = do
    d <- getHomeDirectory
    let filePath = d </> configDirName
    exists <- doesDirectoryExist filePath
    if exists
        then return (Just filePath)
        else return Nothing

hasSavedConfigFile :: FilePath -> IO Bool
hasSavedConfigFile fn = do
    savedConfigFile <- getConfigFilePathForSave fn
    doesFileExist savedConfigFile

-- | Gets a config file. If the second argument is Nothing
-- first looks in the config dir (~/.leksah-x/), if it's not present
-- the data directory at the given location is checked (this is an argument
-- because `getDataDir` has to be called in Leksah code.
getConfigFilePathForLoad :: FilePath -- ^ Config filename with extension
                         -> Maybe FilePath -- ^ Optional directory to check first
                         -> FilePath -- ^ Data dir to check if not present in config dir
                         -> IO FilePath
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

getConfigFilePathForSave :: FilePath -> IO FilePath
getConfigFilePathForSave fn = do
    cd <- getConfigDir
    return (cd </> fn)

allModules :: FilePath -> IO [ModuleName]
allModules filePath = E.catch (do
    exists <- doesDirectoryExist filePath
    if exists
        then do
            filesAndDirs <- getDirectoryContents filePath
            let filesAndDirs' = map (combine filePath)
                                    $filter (\s -> s /= "." && s /= ".." && s /= "_darcs" && s /= "dist" && s /= "dist-newstyle"
                                        && s /= "Setup.lhs") filesAndDirs
            dirs <-  filterM doesDirectoryExist filesAndDirs'
            files <-  filterM doesFileExist filesAndDirs'
            let hsFiles =   filter (\f -> let ext = takeExtension f in
                                            ext == ".hs" || ext == ".lhs") files
            mbModuleStrs <- mapM moduleNameFromFilePath hsFiles
            let mbModuleNames = mapMaybe
                                  (maybe Nothing (simpleParse . T.unpack))
                                  mbModuleStrs
            otherModules <- mapM allModules dirs
            return (mbModuleNames ++ concat otherModules)
        else return [])
            $ \ (_ :: SomeException) -> return []

allHiFiles :: FilePath -> IO [FilePath]
allHiFiles = allFilesWithExtensions [".hi"] True []

allCabalFiles :: FilePath -> IO [FilePath]
allCabalFiles = allFilesWithExtensions [".cabal"] False []

allHaskellSourceFiles :: FilePath -> IO [FilePath]
allHaskellSourceFiles = allFilesWithExtensions [".hs",".lhs"] True []

allFilesWithExtensions :: [FilePath] -> Bool -> [FilePath] -> FilePath -> IO [FilePath]
allFilesWithExtensions extensions recurseFurther collecting filePath = E.catch (do
    exists <- doesDirectoryExist filePath
    if exists
        then do
            filesAndDirs <- getDirectoryContents filePath
            let filesAndDirs' = map (combine filePath)
                                    $filter (\s -> s /= "." && s /= ".." && s /= "_darcs") filesAndDirs
            dirs    <-  filterM doesDirectoryExist filesAndDirs'
            files   <-  filterM doesFileExist filesAndDirs'
            let choosenFiles =   filter (\f -> let ext = takeExtension f in
                                                    elem ext extensions) files
            if recurseFurther || (not recurseFurther && null choosenFiles)
                then foldM (allFilesWithExtensions extensions recurseFurther) (choosenFiles ++ collecting) dirs
                else return (choosenFiles ++ collecting)
        else return collecting)
            $ \ (_ :: SomeException) -> return collecting


moduleNameFromFilePath :: FilePath -> IO (Maybe Text)
moduleNameFromFilePath fp = E.catch (do
    exists <- doesFileExist fp
    if exists
        then do
            str <-  readFile fp
            moduleNameFromFilePath' fp str
        else return Nothing)
            $ \ (_ :: SomeException) -> return Nothing

moduleNameFromFilePath' :: FilePath -> FilePath -> IO (Maybe Text)
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
                Left _ -> return Nothing
                Right str'' -> return (Just str'')

-- | Get the file name to use for the module collector results
-- we want to store the file name for Main module since there can be several in one package
moduleCollectorFileName
    :: ModuleKey -- ^ The module key
    -> String -- ^ The name to use for the collector file (without extension)
moduleCollectorFileName (LibModule name) = display name
moduleCollectorFileName (MainModule sourcePath) =
    "Main_" ++ "_" ++ takeFileName (takeDirectory sourcePath) ++ dropExtension (takeFileName sourcePath)

lexer :: P.TokenParser st
lexer = haskell

lexeme :: CharParser st a -> CharParser st a
lexeme = P.lexeme lexer

whiteSpace :: CharParser st ()
whiteSpace = P.whiteSpace lexer

symbol :: Text -> CharParser st Text
symbol = (T.pack <$>) . P.symbol lexer . T.unpack

moduleNameParser :: CharParser () Text
moduleNameParser = do
    whiteSpace
    _ <- many skipPreproc
    whiteSpace
    _ <- symbol "module"
    lexeme mident
    <?> "module identifier"

skipPreproc :: CharParser () ()
skipPreproc =
    try (do
        whiteSpace
        _ <- char '#'
        _ <- many (noneOf "\n")
        return ())
    <?> "preproc"

mident :: GenParser Char st Text
mident
        = do{ c <- P.identStart haskellDef
            ; cs <- many (alphaNum <|> oneOf "_'.")
            ; return (T.pack (c:cs))
            }
        <?> "midentifier"

findKnownPackages :: FilePath -> IO (Set Text)
findKnownPackages filePath = E.catch (do
    paths           <-  getDirectoryContents filePath
    let nameList    =   map (T.pack . dropExtension) $
            filter (\s -> leksahMetadataSystemFileExtension `isSuffixOf` s) paths
    return (Set.fromList nameList))
        $ \ (_ :: SomeException) -> return Set.empty

isEmptyDirectory :: FilePath -> IO Bool
isEmptyDirectory filePath = E.catch (do
    exists <- doesDirectoryExist filePath
    if exists
        then do
            filesAndDirs <- getDirectoryContents filePath
            return . null $ filter (not . ("." `isPrefixOf`) . takeFileName) filesAndDirs
        else return False)
        (\ (_ :: SomeException) -> return False)

cabalFileName :: FilePath -> IO (Maybe FilePath)
cabalFileName filePath = E.catch (do
    exists <- doesDirectoryExist filePath
    if exists
        then do
            filesAndDirs <- map (filePath </>) <$> getDirectoryContents filePath
            files <-  filterM doesFileExist filesAndDirs
            case filter (\f -> let ext = takeExtension f in ext == ".cabal") files of
                [f] -> return (Just f)
                []  -> return Nothing
                _   -> do
                    warningM "leksah-server" "Multiple cabal files"
                    return Nothing
        else return Nothing)
        (\ (_ :: SomeException) -> return Nothing)

loadNixCache :: MonadIO m => m (Map (FilePath, Text) (Map String String))
loadNixCache = liftIO $ do
    configDir <- getConfigDir
    let filePath = configDir </> "nix.cache"
    doesFileExist filePath >>= \case
        True -> fromMaybe mempty . readMaybe . T.unpack <$> T.readFile filePath
        False -> return mempty

includeInNixCache :: String -> Bool
includeInNixCache name = not (null name)
    && all (\c -> isAlphaNum c || c == '_') name
    && name `notElem` ["POSIXLY_CORRECT", "SHELLOPTS", "BASHOPTS"]

saveNixCache :: MonadIO m => FilePath -> Text -> [ToolOutput] -> m (Map String String)
saveNixCache project compiler out = liftIO $ do
    let newEnv = M.fromList . filter (includeInNixCache . fst) $ mapMaybe (\case
            ToolOutput line -> Just . fixQuotes . second (drop 1) . span (/='=') $ T.unpack line
            _ -> Nothing) out
    configDir <- getConfigDir
    let filePath = configDir </> "nix.cache"
    T.writeFile filePath . T.pack . show =<< M.insert (project, compiler) newEnv <$> loadNixCache
    return newEnv
  where
    fixQuotes ("IFS", _) = ("IFS", " \t\n")
    fixQuotes (n, '\'':rest) = (n, maybe rest T.unpack . T.stripSuffix "'" $ T.pack rest)
    fixQuotes x = x

loadNixEnv :: MonadIO m => FilePath -> Text -> m (Maybe (Map String String))
loadNixEnv project compiler = liftIO $
    (M.lookup (project, compiler) <$> loadNixCache) >>= \case
        Just env -> return (Just env)
        Nothing -> do
            let nixFile = dropFileName project </> "default.nix"
            (out, ph) <- runTool' "nix-shell" [T.pack nixFile, "-A", "shells." <> compiler, "--run", "( set -o posix ; set )"]
                    (Just $ dropFileName project) Nothing
            waitForProcess ph >>= \case
                ExitSuccess -> Just <$> saveNixCache project compiler out
                _ -> return Nothing

runProjectTool :: Maybe FilePath -> FilePath -> [Text] -> Maybe FilePath -> Maybe [(String, String)] -> IO ([ToolOutput], ProcessHandle)
runProjectTool Nothing fp args mbDir mbEnv = runTool' fp args mbDir mbEnv
runProjectTool (Just project) fp args mbDir mbEnv = do
    let nixFile = dropFileName project </> "default.nix"
    doesFileExist nixFile >>= \case
        True ->
            loadNixEnv project "ghc" >>= \case
                Just nixEnv -> do
                    debugM "leksah" $ "Using cached nix environment for " <> show (project, "ghc")
                    runTool' "bash" ["-c", T.pack . showCommandForUser fp $ map T.unpack args] mbDir $ M.toList <$> Just nixEnv <> (M.fromList <$> mbEnv)
                Nothing -> runTool' "nix-shell" [T.pack nixFile, "-A", "shells.ghc", "--run", T.pack . showCommandForUser fp $ map T.unpack args] mbDir mbEnv
        False -> runTool' fp args mbDir mbEnv

getCabalUserPackageDir :: IO (Maybe FilePath)
getCabalUserPackageDir = do
    (!output,_) <- runTool' "cabal" ["help"] Nothing Nothing
    output `deepseq` case T.stripPrefix "  " (toolline $ last output) of
        Just s | "config" `T.isSuffixOf` s -> return . Just . T.unpack $ T.take (T.length s - 6) s <> "packages"
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
    E.catch (do
        exists <- doesDirectoryExist filePath
        when exists $ do
            filesAndDirs             <- getDirectoryContents filePath
            let filesAndDirs'        =  map (combine filePath)
                                            $ filter (\s -> s /= "." && s /= ".." && not ("00-index" `isPrefixOf` s)) filesAndDirs
            dirs                     <- filterM doesDirectoryExist filesAndDirs'
            files                    <- filterM doesFileExist filesAndDirs'
            let choosenFiles         =  filter (isSuffixOf ".tar.gz") files
            let decompressionTargets =  filter (\f -> (dropExtension . dropExtension) f `notElem` dirs) choosenFiles
            mapM_ (\f -> let (dir,fn) = splitFileName f
                             command = "tar -zxf " ++ fn in do
                                setCurrentDirectory dir
                                handle   <- runCommand command
                                _ <- waitForProcess handle
                                return ())
                    decompressionTargets
            mapM_ autoExtractTarFiles' dirs
            return ()
    ) $ \ (_ :: SomeException) -> return ()


getCollectorPath :: MonadIO m => m FilePath
getCollectorPath = liftIO $ do
    configDir <- getConfigDir
    let filePath = configDir </> "metadata"
    createDirectoryIfMissing False filePath
    return filePath

getSysLibDir :: Maybe FilePath -> FilePath -> IO (Maybe FilePath)
getSysLibDir project ver = E.catch (do
    (!output,_) <- runProjectTool project (ghcExeName ver) ["--print-libdir"] Nothing Nothing
    let libDir = listToMaybe [line | ToolOutput line <- output]
        libDir2 = T.dropWhileEnd ((==) 13 . ord) <$> libDir
    output `deepseq` return $ normalise . T.unpack <$> libDir2
    ) $ \ (_ :: SomeException) -> return Nothing

getStackPackages :: FilePath -> IO [(UnitId, Maybe FilePath)]
getStackPackages project = do
    (!output', _) <- runTool' "stack" ["--stack-yaml", T.pack project, "exec", "ghc-pkg", "--", "list", "--simple-output"] Nothing Nothing
    output' `deepseq` return $ map (,Just project) $ concatMap ghcPkgOutputToPackages output'

ghcPkgOutputToPackages :: ToolOutput -> [UnitId]
ghcPkgOutputToPackages (ToolOutput n) = mapMaybe (T.simpleParse . T.unpack) (T.words n)
ghcPkgOutputToPackages _ = []

--getCabalPackages :: FilePath -> FilePath -> IO [(UnitId, [FilePath])]
--getCabalPackages ghcVer project = do
--    packageDBs <- getCabalPackageDBs (Just project) ghcVer
--    (eitherDecodeStrict' <$> BS.readFile (dropFileName project </> "dist-newstyle" </> "cache" </> "plan.json"))
--        >>= \ case
--                Left _ -> return []
--                Right plan -> return . map (,packageDBs) $
--                    mapMaybe (T.simpleParse . T.unpack . piId) (pjPlan plan)

readPlan :: FilePath -> FilePath -> IO (Maybe PlanJson)
readPlan projectRoot buildDir = do
    let distNewstyle = projectRoot </> buildDir
        planFile = distNewstyle </> "cache" </> "plan.json"
    doesFileExist planFile >>= \case
        False -> do
            debugM "leksah" $ "cabal plan not found : " <> planFile
            return Nothing
        True ->
            (eitherDecodeStrict' <$> BS.readFile planFile)
                >>= \ case
                        Right plan -> return (Just plan)
                        Left err -> do
                            errorM "leksah" $ "Error parsing cabal plan : " <> err
                            return Nothing

cabalProjectBuildDir :: FilePath -> FilePath -> IO (FilePath, FilePath -> FilePath -> FilePath, Maybe FilePath)
cabalProjectBuildDir projectRoot buildDir = do
    let distNewstyle = projectRoot </> buildDir
        defaultDir = (distNewstyle </> "build", const $ const "build", Nothing)
    readPlan projectRoot buildDir
        >>= \ case
            Just PlanJson { pjCabalVersion = v } | "1.24." `isPrefixOf` v ->
                return defaultDir
            Just PlanJson
                { pjCabalVersion = v
                , pjCompilerId = Just compilerId
                , pjOS = Just os
                , pjArch = Just arch
                } | "2.0." `isPrefixOf` v -> return (distNewstyle </> "build" </> arch <> "-" <> os </> compilerId,
                        \_ctype component -> "c" </> component </> "build", Just v)
            Just PlanJson
                { pjCabalVersion = v
                , pjCompilerId = Just compilerId
                , pjOS = Just os
                , pjArch = Just arch
                } -> return (distNewstyle </> "build" </> arch <> "-" <> os </> compilerId,
                        \ctype component -> ctype </> component </> "build", Just v)
            Just plan -> do
                errorM "leksah" $ "Unexpected cabal plan : " <> show plan
                return defaultDir
            Nothing -> return defaultDir

getPackages' :: Maybe FilePath -> FilePath -> FilePath -> [FilePath] -> IO [UnitId]
getPackages' project hc hVer packageDBs = do
    (!output', _) <- runProjectTool project (hc <> "-pkg-" <> hVer) (["list", "--simple-output"] ++ map (("--package-db="<>) . T.pack) packageDBs) Nothing Nothing
    output' `deepseq` return $ concatMap ghcPkgOutputToPackages output'

getPackages :: Maybe FilePath -> FilePath -> FilePath-> [FilePath] -> IO [(UnitId, Maybe FilePath)]
getPackages project hc hVer packageDBs =
    map (,project) <$> getPackages' project hc hVer packageDBs

-- | Find the packages that the packages in the workspace
getInstalledPackages :: FilePath -> [FilePath] -> IO [(UnitId, Maybe FilePath)]
getInstalledPackages ghcVer projects = do
--    versions <- nub . (ghcVer:) <$> forM projects (\project -> E.catch (
--            if takeExtension project == "yaml"
--                then return []
--                else do
--                    x <- getProjectCompilerId
--                    projectDB <- getProjectPackageDB ghcVer project "dist-newstyle"
--                    doesDirectoryExist projectDB >>= \case
--                        False -> return []
--                        True  -> map (,globalDBs<>[projectDB]) <$> getPackages' [projectDB]
--        ) $ \ (_ :: SomeException) -> return [])
    globalDBs <- sequence [getGlobalPackageDB Nothing ghcVer, Just <$> getStorePackageDB ghcVer]
        >>= filterM doesDirectoryExist . catMaybes
    globalPackages <- E.catch (getPackages Nothing "ghc" ghcVer globalDBs) $ \ (_ :: SomeException) -> return []
    debugM "leksah" $ "globalPackages = " <> show globalPackages

    localPackages <- forM projects (\project -> E.catch (
            if takeExtension project == "yaml"
                then getStackPackages project
                else do
                    (hc, hVer, projectDB) <- getProjectPackageDB ghcVer project "dist-newstyle"
                    doesDirectoryExist projectDB >>= \case
                        False -> return []
                        True  -> do
                            projGlobalDBs <- sequence [getGlobalPackageDB (Just project) hVer, Just <$> getStorePackageDB hVer]
                                >>= filterM doesDirectoryExist . catMaybes
--                            projGlobalPackages <- E.catch (getPackages (Just project) hc hVer projGlobalDBs) $ \ (_ :: SomeException) -> return []

                            map (,Just project) <$> getPackages' (Just project) hc hVer (projectDB : projGlobalDBs)
        ) $ \ (_ :: SomeException) -> return [])
    debugM "leksah" $ "localPackages = " <> show localPackages
    return . nub $ globalPackages <> concat localPackages

getGlobalPackageDB :: Maybe FilePath -> FilePath -> IO (Maybe FilePath)
getGlobalPackageDB mbProject ghcVersion = do
    ghcLibDir <- getSysLibDir mbProject ghcVersion
--    case mbProject of
--        Nothing -> getSysLibDir ghcVersion
--        Just project -> do
--            let nixFile = dropFileName project </> "default.nix"
--            doesFileExist nixFile >>= \case
--                False -> getSysLibDir ghcVersion
--                True -> getSysLibDirNix nixFile >>= \case
--                    Nothing -> getSysLibDir ghcVersion
--                    Just d -> return $ Just d
    return $ (</> "package.conf.d") <$> ghcLibDir

getStorePackageDB :: FilePath -> IO FilePath
getStorePackageDB ghcVersion = do
    cabalDir <- getAppUserDataDirectory "cabal"
    return $ cabalDir </> "store" </> "ghc-" ++ ghcVersion </> "package.db"

getProjectPackageDB :: FilePath -> FilePath -> FilePath -> IO (FilePath, FilePath, FilePath)
getProjectPackageDB ghcVersion project buildDir = do
    let projectRoot = dropFileName project
    c <- fromMaybe ("ghc-" <> ghcVersion) <$> getProjectCompilerId projectRoot buildDir
    let (hc, hVer) = span (/='-') c
    return (hc, drop 1 hVer, projectRoot </> buildDir </> "packagedb" </> c)

getProjectCompilerId :: FilePath -> FilePath -> IO (Maybe FilePath)
getProjectCompilerId projectRoot buildDir =
    join . mapM pjCompilerId <$> readPlan projectRoot buildDir

--getProjectCompiler :: FilePath -> FilePath -> IO (Maybe FilePath)
--getProjectCompiler projectRoot buildDir =
--    getProjectCompilerId projectRoot buildDir >>= \case
--        Just PlanJson { pjCompilerId = Just c } -> return . Just . dropWhile (=='-') $ dropWhile (/='-') c
--        _ -> return Nothing

getCabalPackageDBs :: Maybe FilePath -> FilePath -> IO [FilePath]
getCabalPackageDBs Nothing ghcVersion =
    sequence [getGlobalPackageDB Nothing ghcVersion, Just <$> getStorePackageDB ghcVersion]
        >>= filterM doesDirectoryExist . catMaybes
getCabalPackageDBs (Just project) ghcVersion = do
    (_, hVer, projectDB) <- getProjectPackageDB ghcVersion project "dist-newstyle"
    doesDirectoryExist projectDB >>= \case
        False -> return []
        True  -> do
            projGlobalDBs <- sequence [getGlobalPackageDB (Just project) hVer, Just <$> getStorePackageDB hVer]
                >>= filterM doesDirectoryExist . catMaybes
            return (projectDB : projGlobalDBs)

getStackPackageDBs :: FilePath -> IO [FilePath]
getStackPackageDBs project = do
    (!output, _) <- runTool' "stack" ["--stack-yaml", T.pack project, "path", "--ghc-package-path"] Nothing Nothing
    filterM doesDirectoryExist $ concatMap paths output
  where
    paths (ToolOutput n) = map T.unpack (T.splitOn ":" n)
    paths _ = []

getPackageDBs' :: FilePath -> Maybe FilePath -> IO [FilePath]
getPackageDBs' ghcVersion mbProject =
    E.catch (
        case mbProject of
            Just project | takeExtension project == "yaml" ->
                getStackPackageDBs project
            _ -> getCabalPackageDBs mbProject ghcVersion
     ) $ \ (_ :: SomeException) -> return []

getPackageDBs :: [FilePath] -> IO [(Maybe FilePath, [FilePath])]
getPackageDBs projects = do
    ghcVersion <- getDefaultGhcVersion
    globalDBs <- sequence [getGlobalPackageDB Nothing ghcVersion, Just <$> getStorePackageDB ghcVersion]
        >>= filterM doesDirectoryExist . catMaybes
    nub . ((Nothing, globalDBs):) <$> forM projects (\p -> (Just p,) <$> getPackageDBs' ghcVersion (Just p))

figureOutHaddockOpts :: Maybe FilePath -> FilePath -> IO [Text]
figureOutHaddockOpts mbProject package = do
    (!output,_) <- runProjectTool mbProject "cabal" ["haddock", "--with-haddock=leksahecho", "--executables"] (Just $ dropFileName package) Nothing
    let opts = concat [words $ T.unpack l | ToolOutput l <- output]
    let res = filterOptGhc opts
    debugM "leksah-server" ("figureOutHaddockOpts " ++ show res)
    output `deepseq` return $ map T.pack res
    where
        filterOptGhc :: [String] -> [String]
        filterOptGhc []    = []
        filterOptGhc (s:r) = case stripPrefix "--optghc=" s of
                                    Nothing -> filterOptGhc r
                                    Just s'  -> s' : filterOptGhc r

figureOutGhcOpts :: Maybe FilePath -> FilePath -> IO [Text]
figureOutGhcOpts mbProject package = do
    debugM "leksah-server" "figureOutGhcOpts"
    ghcVersion <- getDefaultGhcVersion
    packageDBs <- liftIO $ getPackageDBs' ghcVersion mbProject
    let flags = if takeBaseName package == "base"
                    then ["-finteger-gmp", "-finteger-gmp2"]
                    else ["-f-enable-overloading", "-f-overloaded-methods", "-f-overloaded-properties", "-f-overloaded-signals"]
    (!output,_) <- runProjectTool mbProject "cabal" ("configure" : flags <> map (("--package-db=" <>) . T.pack) packageDBs) (Just $ dropFileName package) Nothing
    output `deepseq` figureOutGhcOpts' mbProject package

figureOutGhcOpts' :: Maybe FilePath -> FilePath -> IO [Text]
figureOutGhcOpts' mbProject package = do
    debugM "leksah-server" "figureOutGhcOpts'"
    (!output,_) <- runProjectTool mbProject "cabal" ["build","--with-ghc=leksahecho","--with-ghcjs=leksahecho"] (Just $ dropFileName package) Nothing
    let res = case catMaybes [findMake $ T.unpack l | ToolOutput l <- output] of
                options:_ -> words options
                _         -> []
    debugM "leksah-server" ("figureOutGhcOpts " ++ show res)
    output `deepseq` return $ map T.pack res
  where
    findMake :: String -> Maybe String
    findMake [] = Nothing
    findMake line@(_:xs) =
            case stripPrefix "--make " line of
                Nothing -> findMake xs
                s -> s
