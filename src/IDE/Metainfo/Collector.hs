{-# OPTIONS_GHC -XScopedTypeVariables -fno-warn-type-defaults #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main (
    main
) where

import System.Console.GetOpt
    (ArgDescr(..), usageInfo, ArgOrder(..), getOpt, OptDescr(..))
import System.Environment (getArgs)
import Control.Monad (when)
import Data.Version (showVersion)
import Paths_leksah_server (getDataDir, version)
import qualified Data.Map as Map
import Data.List(nub,delete)
import IDE.Utils.FileUtils
import IDE.Utils.Utils
import IDE.Metainfo.InterfaceCollector
import IDE.Utils.GHCUtils
import IDE.StrippedPrefs
import IDE.Metainfo.WorkspaceCollector
import Data.Maybe(catMaybes, fromJust, mapMaybe, isJust)
import Distribution.Text (display)
import Prelude hiding(catch)
import Debug.Trace
import Control.Monad (liftM)
import System.Directory (removeDirectoryRecursive, doesFileExist, removeFile, doesDirectoryExist, setCurrentDirectory)
import qualified Data.Set as Set (member)
import IDE.Core.CTypes hiding (Extension)
import qualified Distribution.InstalledPackageInfo as IPI
import PackageConfig (PackageConfig)
import TcRnMonad (MonadIO(..))
import System.FilePath ((<.>), (</>))
import IDE.Metainfo.SourceCollectorH
       (PackageCollectStats(..), collectPackageFromSource)
import Data.Binary.Shared (encodeFileSer)
import IDE.Metainfo.SourceDB (buildSourceForPackageDB)
import Data.Time
import Control.Exception
       (catch, SomeException)
import MyMissing(trim)
import System.Log
import System.Log.Logger(updateGlobalLogger,rootLoggerName,addHandler,debugM,infoM,errorM,
    setLevel)
import System.Log.Handler.Simple(fileHandler)
import Network(withSocketsDo)
import Network.Socket (SocketType(..), iNADDR_ANY, SockAddr(..),PortNumber(..))
import IDE.Utils.Server
import System.IO (Handle, hPutStrLn, hGetLine, hFlush, hClose)
import IDE.HeaderParser(parseTheHeader)
import System.Process (system)
import System.Exit (ExitCode(..))
import Distribution.Package (PackageIdentifier(..))

-- --------------------------------------------------------------------
-- Command line options
--

getThisPackage :: PackageConfig -> PackageIdentifier
#if MIN_VERSION_Cabal(1,8,0)
getThisPackage    =   IPI.sourcePackageId
#else
getThisPackage    =   IPI.package
#endif

data Flag =    CollectSystem

             | ServerCommand (Maybe String)
             --modifiers
             | Rebuild
             | Sources
             -- | Directory FilePath
             --others
             | VersionF
             | Help
             | Debug
             | Verbosity String
             | LogFile String
       deriving (Show,Eq)

options :: [OptDescr Flag]

options =   [
-- main functions
             Option ['s'] ["system"] (NoArg CollectSystem)
                "Collects new information for installed packages"
         ,   Option ['r'] ["server"] (OptArg ServerCommand "Maybe Port")
                "Start as server."
         ,   Option ['b'] ["rebuild"] (NoArg Rebuild)
                "Modifier for -s and -p: Rebuild metadata"
         ,   Option ['o'] ["sources"] (NoArg Sources)
                "Modifier for -s: Gather info about pathes to sources"
         ,   Option ['v'] ["version"] (NoArg VersionF)
                "Show the version number of ide"
         ,   Option ['h'] ["help"] (NoArg Help)
                "Display command line options"
         ,   Option ['d'] ["debug"] (NoArg Debug)
                "Write ascii pack files"
         ,   Option ['e'] ["verbosity"] (ReqArg Verbosity "Verbosity")
                "One of DEBUG, INFO, NOTICE, WARNING, ERROR, CRITICAL, ALERT, EMERGENCY"
         ,   Option ['l'] ["logfile"] (ReqArg LogFile "LogFile")
                "File path for logging messages"
    ]

header :: String
header = "Usage: leksah-collector [OPTION...] files..."

ideOpts :: [String] -> IO ([Flag], [String])
ideOpts argv =
    case getOpt Permute options argv of
          (o,n,[]  ) -> return (o,n)
          (_,_,errs) -> ioError $ userError $ concat errs ++ usageInfo header options

-- ---------------------------------------------------------------------
-- | Main function
--

main :: IO ()
main =  withSocketsDo $ catch inner handler
    where
        handler (e :: SomeException) = do
            putStrLn $ "leksah-server: " ++ (show e)
            errorM "leksah-server" (show e)
            return ()
        inner = do
            args            <- getArgs
            (o,_)           <- ideOpts args
            _fp             <- getConfigFilePathForSave "collectorl.lkslo"
            let verbosity'   =  catMaybes $
                                    map (\x -> case x of
                                        Verbosity s -> Just s
                                        _           -> Nothing) o
            let verbosity    =  case verbosity' of
                                    [] -> INFO
                                    h:_ -> read h
            let logFile'     =  catMaybes $
                                    map (\x -> case x of
                                        LogFile s   -> Just s
                                        _           -> Nothing) o
            let logFile     =  case logFile' of
                                    [] -> Nothing
                                    h:_ -> Just h
            updateGlobalLogger rootLoggerName (\ l -> setLevel verbosity l)
            when (isJust logFile) $  do
                handler' <- fileHandler (fromJust logFile) verbosity
                updateGlobalLogger rootLoggerName (\ l -> addHandler handler' l)
            infoM "leksah-server" $ "***server start"
            debugM "leksah-server" $ "args: " ++ show args
            dataDir         <- getDataDir
            prefsPath       <- getConfigFilePathForLoad strippedPreferencesFilename Nothing dataDir
            prefs           <- readStrippedPrefs prefsPath
            debugM "leksah-server" $ "prefs " ++ show prefs

            if elem VersionF o
                then putStrLn $ "Leksah Haskell IDE (server), version " ++ showVersion version
                else if elem Help o
                    then putStrLn $ "Leksah Haskell IDE (server) " ++ usageInfo header options
                    else do
                        let servers     =   catMaybes $
                                                map (\x -> case x of
                                                                ServerCommand s -> Just s
                                                                _        -> Nothing) o
                        let sources     =   elem Sources o
                        let rebuild     =   elem Rebuild o
                        let debug       =   elem Debug o
                        if elem CollectSystem o
                            then do
                                debugM "leksah-server" "collectSystem"
                                collectSystem prefs debug rebuild sources
                            else
                                case servers of
                                    (Nothing:_)  -> do
                                        running <- serveOne Nothing (server (PortNum (fromIntegral (serverPort prefs))) prefs)
                                        waitFor running
                                        return ()
                                    (Just ps:_)  -> do
                                        let port = read ps
                                        running <- serveOne Nothing (server (PortNum (fromIntegral port)) prefs)
                                        waitFor running
                                        return ()
                                    _ -> return ()

        server port prefs = Server (SockAddrInet port iNADDR_ANY) Stream (doCommands prefs)

doCommands :: Prefs -> (Handle, t1, t2) -> IO ()
doCommands prefs (h,n,p) = do
    debugM "leksah-server" $ "***wait"
    mbLine <- catch (liftM Just (hGetLine h))
                (\ (_e :: SomeException) -> do
                    infoM "leksah-server" $ "***lost connection"
                    hClose h
                    return Nothing)
    case mbLine of
        Nothing -> return ()
        Just line -> do
            case read line of
                    SystemCommand rebuild sources _extract -> --the extract arg is not used
                        catch (do
                            collectSystem prefs False rebuild sources
                            hPutStrLn h (show ServerOK)
                            hFlush h)
                        (\ (e :: SomeException) -> do
                            hPutStrLn h (show (ServerFailed (show e)))
                            hFlush h)
                    WorkspaceCommand rebuild package path modList ->
                        catch (do
                            collectWorkspace package modList rebuild False path
                            hPutStrLn h (show ServerOK)
                            hFlush h)
                        (\ (e :: SomeException) -> do
                            hPutStrLn h (show (ServerFailed (show e)))
                            hFlush h)
                    ParseHeaderCommand filePath ->
                        catch (do
                            res <- parseTheHeader filePath
                            hPutStrLn h (show res)
                            hFlush h)
                        (\ (e :: SomeException) -> do
                            hPutStrLn h (show (ServerFailed (show e)))
                            hFlush h)
            doCommands prefs (h,n,p)

collectSystem :: Prefs -> Bool -> Bool -> Bool -> IO()
collectSystem prefs writeAscii forceRebuild findSources = do
    collectorPath       <- getCollectorPath
    when forceRebuild $ do
        exists           <- doesDirectoryExist collectorPath
        when exists $ removeDirectoryRecursive collectorPath
        reportPath       <-  getConfigFilePathForSave "collectSystem.report"
        exists'          <- doesFileExist reportPath
        when exists' (removeFile reportPath)
        return ()
    knownPackages       <-  findKnownPackages collectorPath
    debugM "leksah-server" $ "collectSystem knownPackages= " ++ show knownPackages
    packageInfos        <-  inGhcIO [] [] $  \ _ -> getInstalledPackageInfos
    debugM "leksah-server" $ "collectSystem packageInfos= " ++ show (map IPI.package packageInfos)
    let newPackages     =   filter (\pid -> not $Set.member (packageIdentifierToString $ getThisPackage pid)
                                                         knownPackages)
                                    packageInfos
    if null newPackages
        then do
            infoM "leksah-server" "Metadata collector has nothing to do"
        else do
            when findSources $ liftIO $ buildSourceForPackageDB prefs
            stats <- mapM (collectPackage writeAscii prefs) newPackages
            writeStats stats
    infoM "leksah-server" "Metadata collection has finished"

writeStats :: [PackageCollectStats] -> IO ()
writeStats stats = do
    reportPath       <-  getConfigFilePathForSave "collectSystem.report"
    time             <-  getCurrentTime
    appendFile reportPath (report time)
    where
        report time = "\n++++++++++++++++++++++++++++++\n" ++ show time ++ "\n++++++++++++++++++++++++++++++\n"
                        ++ header' time ++ summary ++ details
        header' _time = "\nLeksah system metadata collection "
        summary = "\nSuccess with         = " ++ packs ++
                  "\nPackages total       = " ++ show packagesTotal ++
                  "\nPackages with source = " ++ show packagesWithSource ++
                  "\nPackages retreived   = " ++ show packagesRetreived ++
                  "\nModules total        = " ++ show modulesTotal' ++
                  "\nModules with source  = " ++ show modulesWithSource ++
                  "\nPercentage source    = " ++ show percentageWithSource
        packagesTotal        = length stats
        packagesWithSource   = length (filter withSource stats)
        packagesRetreived    = length (filter retrieved stats)
        modulesTotal'        = sum (mapMaybe modulesTotal stats)
        modulesWithSource    = sum (mapMaybe modulesTotal (filter withSource stats))
        percentageWithSource = (fromIntegral modulesWithSource) * 100.0 /
                                    (fromIntegral modulesTotal')
        details              = foldr detail "" (filter (isJust . mbError) stats)
        detail stat string   = string ++ "\n" ++ packageString stat ++ " " ++ trim (fromJust (mbError stat))
        packs                = foldr (\stat string -> string ++ packageString stat ++ " ")
                                        "" (take 10 (filter withSource stats))
                                        ++ if packagesWithSource > 10 then "..." else ""


collectPackage :: Bool -> Prefs -> PackageConfig -> IO PackageCollectStats
collectPackage writeAscii prefs packageConfig = trace ("collectPackage " ++ display (getThisPackage packageConfig))
    $ do
    packageDescrHI          <- collectPackageFromHI packageConfig
    let packString = packageIdentifierToString (pdPackage packageDescrHI)
    mbPackageDescrPair      <- collectPackageFromSource prefs packageConfig
    case mbPackageDescrPair of
        (Nothing,stat, Just fp) ->  do
            -- Try to retreive prebuild package
            case retreiveURL prefs of
                Just url -> do
                    collectorPath   <- liftIO $ getCollectorPath
                    setCurrentDirectory collectorPath
                    let fullUrl = url ++ "/metadata-" ++ leksahVersion ++ "/" ++ packString ++ leksahMetadataSystemFileExtension
                    debugM "leksah-server" $ "collectPackage: before retreiving = " ++ fullUrl
                    catch (system $ "wget " ++ fullUrl)
                        (\(e :: SomeException) -> do
                            debugM "leksah-server" $ "collectPackage: Error when calling wget " ++ show e
                            return (ExitFailure 1))
                    debugM "leksah-server" $ "collectPackage: after retreiving = " ++ packString -- ++ " result = " ++ res
                    let filePath    =  collectorPath </> packString <.> leksahMetadataSystemFileExtension
                    exist <- doesFileExist filePath
                    if exist
                        then do
                            debugM "leksah-server" $ "collectPackage: retreived = " ++ packString
                            liftIO $ writePackagePath fp packageDescrHI
                            return (stat {modulesTotal = Just (length (pdModules packageDescrHI)),
                                    withSource=True, retrieved= True, mbError=Nothing})
                        else do
                            debugM "leksah-server" $ "collectPackage: Can't retreive = " ++ packString
                            liftIO $ writeExtractedPackage False packageDescrHI
                            return (stat {modulesTotal = Just (length (pdModules packageDescrHI))})
                Nothing -> do
                    liftIO $ writeExtractedPackage False packageDescrHI
                    return (stat {modulesTotal = Just (length (pdModules packageDescrHI))})
        (Just packageDescrS,stat, Just fp) ->  do
            let mergedPackageDescr = mergePackageDescrs packageDescrHI packageDescrS
            liftIO $ writeExtractedPackage writeAscii mergedPackageDescr
            liftIO $ writePackagePath fp mergedPackageDescr
            return (stat)
        (Nothing,stat,Nothing) ->  do
            liftIO $ writeExtractedPackage False packageDescrHI
            return (stat {modulesTotal = Just (length (pdModules packageDescrHI))})
        _ -> fail "Unexpected error in collectPackage"

writeExtractedPackage :: MonadIO m => Bool -> PackageDescr -> m ()
writeExtractedPackage writeAscii pd = do
    collectorPath   <- liftIO $ getCollectorPath
    let filePath    =  collectorPath </> packageIdentifierToString (pdPackage pd) <.>
                            leksahMetadataSystemFileExtension
    if writeAscii
        then liftIO $ writeFile (filePath ++ "dpg") (show pd)
        else liftIO $ encodeFileSer filePath (metadataVersion, pd)

writePackagePath :: MonadIO m => FilePath -> PackageDescr -> m ()
writePackagePath fp pd = do
    collectorPath   <- liftIO $ getCollectorPath
    let filePath    =  collectorPath </> packageIdentifierToString (pdPackage pd) <.>
                            leksahMetadataPathFileExtension
    liftIO $ writeFile filePath fp

--------------Merging of .hi and .hs parsing / parsing and typechecking results

mergePackageDescrs :: PackageDescr -> PackageDescr -> PackageDescr
mergePackageDescrs packageDescrHI packageDescrS = PackageDescr {
        pdPackage           =   pdPackage packageDescrHI
    ,   pdMbSourcePath      =   pdMbSourcePath packageDescrS
    ,   pdModules           =   mergeModuleDescrs (pdModules packageDescrHI) (pdModules packageDescrS)
    ,   pdBuildDepends      =   pdBuildDepends packageDescrHI}

mergeModuleDescrs :: [ModuleDescr] -> [ModuleDescr] -> [ModuleDescr]
mergeModuleDescrs hiList srcList =  trace ("mergeModuleDescrs allNames" ++ show allNames)
                                        $ map mergeIt allNames
    where
        mergeIt :: String -> ModuleDescr
        mergeIt str = case (Map.lookup str hiDict, Map.lookup str srcDict) of
                        (Just mdhi, Nothing) -> mdhi
                        (Nothing, Just mdsrc) -> mdsrc
                        (Just mdhi, Just mdsrc) -> mergeModuleDescr mdhi mdsrc
                        (Nothing, Nothing) -> error "Collector>>mergeModuleDescrs: impossible"
        allNames = nub $ Map.keys hiDict ++  Map.keys srcDict
        hiDict = Map.fromList $ zip ((map (display . modu . mdModuleId)) hiList) hiList
        srcDict = Map.fromList $ zip ((map (display . modu . mdModuleId)) srcList) srcList

mergeModuleDescr :: ModuleDescr -> ModuleDescr -> ModuleDescr
mergeModuleDescr hiDescr srcDescr = ModuleDescr {
        mdModuleId          = mdModuleId hiDescr
    ,   mdMbSourcePath      = mdMbSourcePath srcDescr
    ,   mdReferences        = mdReferences hiDescr
    ,   mdIdDescriptions    = mergeDescrs (mdIdDescriptions hiDescr) (mdIdDescriptions srcDescr)}

mergeDescrs :: [Descr] -> [Descr] -> [Descr]
mergeDescrs hiList srcList =  concatMap mergeIt allNames
    where
        mergeIt :: String -> [Descr]
        mergeIt pm = case (Map.lookup pm hiDict, Map.lookup pm srcDict) of
                        (Just mdhi, Nothing) -> mdhi
                        (Nothing, Just mdsrc) -> mdsrc
                        (Just mdhi, Just mdsrc) -> map (\ (a,b) -> mergeDescr a b) $ makePairs mdhi mdsrc
                        (Nothing, Nothing) -> error "Collector>>mergeModuleDescrs: impossible"
        allNames   = nub $ Map.keys hiDict ++  Map.keys srcDict
        hiDict     = Map.fromListWith (++) $ zip ((map dscName) hiList) (map (\ e -> [e]) hiList)
        srcDict    = Map.fromListWith (++) $ zip ((map dscName) srcList)(map (\ e -> [e]) srcList)

makePairs :: [Descr] -> [Descr] -> [(Maybe Descr,Maybe Descr)]
makePairs (hd:tl) srcList = (Just hd, theMatching)
                            : makePairs tl (case theMatching of
                                                Just tm -> delete tm srcList
                                                Nothing -> srcList)
    where
        theMatching          = findMatching hd srcList
        findMatching ele (hd':tail')
            | matches ele hd' = Just hd'
            | otherwise       = findMatching ele tail'
        findMatching _ele []  = Nothing
        matches :: Descr -> Descr -> Bool
        matches d1 d2 = (descrType . dscTypeHint) d1 == (descrType . dscTypeHint) d2
makePairs [] rest = map (\ a -> (Nothing, Just a)) rest

mergeDescr :: Maybe Descr -> Maybe Descr -> Descr
mergeDescr (Just descr) Nothing = descr
mergeDescr Nothing (Just descr) = descr
mergeDescr (Just (Real rdhi)) (Just (Real rdsrc)) =
    Real RealDescr {
        dscName'        = dscName' rdhi
    ,   dscMbTypeStr'   = dscMbTypeStr' rdhi
    ,   dscMbModu'      = dscMbModu' rdsrc
    ,   dscMbLocation'  = dscMbLocation' rdsrc
    ,   dscMbComment'   = dscMbComment' rdsrc
    ,   dscTypeHint'    = mergeTypeDescr (dscTypeHint' rdhi) (dscTypeHint' rdsrc)
    ,   dscExported'    = True
    }
mergeDescr (Just (Reexported rdhi)) (Just rdsrc) =
    Reexported $ ReexportedDescr {
        dsrMbModu       = dsrMbModu rdhi
    ,   dsrDescr        = mergeDescr (Just (dsrDescr rdhi)) (Just rdsrc)
    }
mergeDescr _ _ =  error "Collector>>mergeDescr: impossible"

mergeTypeHint :: Maybe TypeDescr -> Maybe TypeDescr -> Maybe TypeDescr
mergeTypeHint Nothing Nothing         = Nothing
mergeTypeHint Nothing jtd             = jtd
mergeTypeHint jtd Nothing             = jtd
mergeTypeHint (Just tdhi) (Just tdhs) = Just (mergeTypeDescr tdhi tdhs)

mergeTypeDescr :: TypeDescr -> TypeDescr -> TypeDescr
mergeTypeDescr (DataDescr constrListHi fieldListHi) (DataDescr constrListSrc fieldListSrc) =
    DataDescr (mergeSimpleDescrs constrListHi constrListSrc) (mergeSimpleDescrs fieldListHi fieldListSrc)
mergeTypeDescr (NewtypeDescr constrHi mbFieldHi) (NewtypeDescr constrSrc mbFieldSrc)       =
    NewtypeDescr (mergeSimpleDescr constrHi constrSrc) (mergeMbDescr mbFieldHi mbFieldSrc)
mergeTypeDescr (ClassDescr superHi methodsHi) (ClassDescr _superSrc methodsSrc)            =
    ClassDescr superHi (mergeSimpleDescrs methodsHi methodsSrc)
mergeTypeDescr (InstanceDescr _bindsHi) (InstanceDescr bindsSrc)                           =
    InstanceDescr bindsSrc
mergeTypeDescr descrHi _                                                                   =
    descrHi

mergeSimpleDescrs :: [SimpleDescr] -> [SimpleDescr] -> [SimpleDescr]
mergeSimpleDescrs hiList srcList =  map mergeIt allNames
    where
        mergeIt :: String -> SimpleDescr
        mergeIt pm = case mergeMbDescr (Map.lookup pm hiDict) (Map.lookup pm srcDict) of
                        Just mdhi -> mdhi
                        Nothing   -> error "Collector>>mergeSimpleDescrs: impossible"
        allNames   = nub $ Map.keys hiDict ++  Map.keys srcDict
        hiDict     = Map.fromList $ zip ((map sdName) hiList) hiList
        srcDict    = Map.fromList $ zip ((map sdName) srcList) srcList

mergeSimpleDescr :: SimpleDescr -> SimpleDescr -> SimpleDescr
mergeSimpleDescr sdHi sdSrc = SimpleDescr {
    sdName      = sdName sdHi,
    sdType      = sdType sdHi,
    sdLocation  = sdLocation sdSrc,
    sdComment   = sdComment sdSrc,
    sdExported  = sdExported sdSrc}

mergeMbDescr :: Maybe SimpleDescr -> Maybe SimpleDescr -> Maybe SimpleDescr
mergeMbDescr (Just mdhi) Nothing      =  Just mdhi
mergeMbDescr Nothing (Just mdsrc)     =  Just mdsrc
mergeMbDescr (Just mdhi) (Just mdsrc) =  Just (mergeSimpleDescr mdhi mdsrc)
mergeMbDescr Nothing Nothing          =  Nothing



