{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Metainfo.PackageCollector
-- Copyright   :  2007-2009 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL Nothing
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Metainfo.PackageCollector (

    collectPackage
  , collectPackageOnly

) where

import Prelude ()
import Prelude.Compat
import IDE.StrippedPrefs
       (getUnpackDirectory, RetrieveStrategy(..), Prefs(..))
import PackageConfig (PackageConfig)
import IDE.Metainfo.SourceCollectorH
       (findSourceForPackage, packageFromSource, PackageCollectStats(..))
import System.Log.Logger (errorM, debugM, infoM)
import IDE.Metainfo.InterfaceCollector (collectPackageFromHI)
import IDE.Core.CTypes
       (dscTypeHint, descrType, dscName, modu, sdExported, sdComment,
        sdLocation, sdType, sdName, SimpleDescr(..), TypeDescr(..),
        dsrDescr, dsrMbModu, ReexportedDescr(..), Descr(..), dscExported',
        dscTypeHint', dscMbComment', dscMbLocation', dscMbModu',
        dscMbTypeStr', dscName', RealDescr(..), Descr, metadataVersion,
        PackageDescr(..), leksahVersion, packageIdentifierToString,
        getThisPackage, packId, ModuleDescr(..))
import IDE.Utils.FileUtils (runProjectTool, getCollectorPath, getSysLibDir)
import System.Directory
       (createDirectoryIfMissing, doesDirectoryExist, doesFileExist,
        setCurrentDirectory)
import IDE.Utils.Utils
       (leksahMetadataPathFileExtension,
        leksahMetadataSystemFileExtension)
import System.FilePath
       (takeDirectory, dropFileName, takeBaseName, (<.>), (</>))
import Data.Binary.Shared (encodeFileSer)
import Distribution.Text (display)
import Control.Monad.IO.Class (MonadIO, MonadIO(..))
import qualified Control.Exception as E (SomeException, catch)
import IDE.Utils.Tool (ToolOutput(..), runTool')
import qualified Data.Text as T
       (stripSuffix, stripPrefix, unpack, pack)
import Data.Text (Text)
import Network.HTTP.Proxy (Proxy(..), fetchProxy)
import Network.Browser
       (request, setAuthorityGen, setOutHandler, setErrHandler, setProxy,
        browse)
import Data.Char (isSpace)
import Network.URI (parseURI)
import Network.HTTP (rspBody, rspCode, Header(..), Request(..))
import Network.HTTP.Base (RequestMethod(..))
import Network.HTTP.Headers (HeaderName(..))
import qualified Data.ByteString as BS (readFile, writeFile, empty)
import qualified Paths_leksah_server (version)
import Distribution.System (buildArch, buildOS)
import qualified Data.Map as Map
       (fromListWith, fromList, keys, lookup)
import Data.List (delete, nub)
import GHC.IO.Exception (ExitCode(..))
import Distribution.Package (pkgName)
import Distribution.Simple.Utils (installDirectoryContents)
import Distribution.Verbosity (normal)
import Data.Maybe (fromMaybe, maybeToList)
import Paths_leksah_server (getDataDir)

collectPackage :: Bool -> Prefs -> Int -> ((PackageConfig, (Maybe FilePath, [FilePath])), Int) -> IO PackageCollectStats
collectPackage writeAscii prefs numPackages ((packageConfig, (mbProject, dbs)), packageIndex) =
    collectPackageNix prefs packageConfig mbProject >>= \case
        Just s -> return s
        Nothing -> do
            infoM "leksah-server" ("update_toolbar " ++ show
                ((fromIntegral packageIndex / fromIntegral numPackages) :: Double))
            debugM "leksah-server" $ "collectPackage (mbProject, dbs) " <> show (mbProject, dbs)
            eitherStrFp    <- findSourceForPackage prefs pid mbProject
            case eitherStrFp of
                Left message -> do
                    debugM "leksah-server" . T.unpack $ message <> " : " <> packageName
                    collectPackageFromHI mbProject packageConfig dbs >>= \case
                        Just packageDescrHi -> do
                            writeExtractedPackage False packageDescrHi
                            return stat {packageString = message, modulesTotal = Just (length (pdModules packageDescrHi))}
                        Nothing -> return stat
                Right fpSource ->
                    case retrieveStrategy prefs of
                        RetrieveThenBuild ->
                            retrieve fpSource >>= \case
                                Just stats -> return stats
                                Nothing -> buildOnly fpSource
                        BuildThenRetrieve -> do
                            debugM "leksah-server" $ "Build (then retrieve) " <> T.unpack packageName <> " in " <> fpSource
                            build fpSource >>= \case
                                (Nothing, bstat) -> return bstat
                                (Just packageDescrHi, bstat) ->
                                    retrieve fpSource >>= \case
                                        Just stats -> return stats
                                        Nothing -> do
                                            writeExtractedPackage False packageDescrHi
                                            return bstat{modulesTotal = Just (length (pdModules packageDescrHi))}
                        NeverRetrieve -> do
                            debugM "leksah-server" $ "Build " <> T.unpack packageName <> " in " <> fpSource
                            buildOnly fpSource
    where
        pid = packId $ getThisPackage packageConfig
        packageName = packageIdentifierToString pid
        stat = PackageCollectStats packageName Nothing False False Nothing
        retrieve :: FilePath -> IO (Maybe PackageCollectStats)
        retrieve fpSource = do
            collectorPath   <- liftIO getCollectorPath
            setCurrentDirectory collectorPath
            let fullUrl  = T.unpack (retrieveURL prefs) <> "/metadata-" <> leksahVersion <> "/" <> T.unpack packageName <> leksahMetadataSystemFileExtension
                filePath = collectorPath </> T.unpack packageName <.> leksahMetadataSystemFileExtension

            case parseURI fullUrl of
                Nothing -> do
                    errorM "leksah-server" $ "collectPackage: invalid URI = " <> fullUrl
                    return Nothing
                Just uri -> do
                    debugM "leksah-server" $ "collectPackage: before retreiving = " <> fullUrl
                    proxy <- filterEmptyProxy . trimProxyUri <$> fetchProxy True
                    (_, rsp) <- browse $ do
                        setProxy proxy
                        setErrHandler (errorM "leksah-server")
                        setOutHandler (debugM "leksah-server")
                        setAuthorityGen (\_ _ -> return Nothing)
                        request Request{ rqURI      = uri
                                        , rqMethod  = GET
                                        , rqHeaders = [Header HdrUserAgent userAgent]
                                        , rqBody    = BS.empty }
                    if rspCode rsp == (2,0,0)
                        then do
                            BS.writeFile filePath $ rspBody rsp
                            debugM "leksah-server" . T.unpack $ "collectPackage: retreived = " <> packageName
                            liftIO $ writePackagePath (dropFileName fpSource) packageName
                            return (Just stat {withSource=True, retrieved= True, mbError=Nothing})
                        else do
                            debugM "leksah-server" . T.unpack $ "collectPackage: Can't retreive = " <> packageName
                            return Nothing

        build :: FilePath -> IO (Maybe PackageDescr, PackageCollectStats)
        build fpSource =
            collectPackageFromHI mbProject packageConfig dbs >>= \case
                Nothing -> return (Nothing, stat)
                Just packageDescrHi -> do
                    runCabalConfigure fpSource
                    mbPackageDescrPair <- packageFromSource mbProject dbs fpSource packageConfig
                    case mbPackageDescrPair of
                        (Just packageDescrS, bstat) -> do
                            writeMerged packageDescrS packageDescrHi fpSource
                            return (Nothing, bstat{modulesTotal = Just (length (pdModules packageDescrS))})
                        (Nothing, bstat) -> return (Just packageDescrHi, bstat)

        buildOnly :: FilePath -> IO PackageCollectStats
        buildOnly fpSource =
            build fpSource >>= \case
                (Nothing, bstat) -> return bstat
                (Just packageDescrHi, bstat) -> do
                    writeExtractedPackage False packageDescrHi
                    return bstat{modulesTotal = Just (length (pdModules packageDescrHi))}

        trimProxyUri (Proxy uri auth) = Proxy (trim uri) auth
        trimProxyUri p = p
        filterEmptyProxy (Proxy "" _) = NoProxy
        filterEmptyProxy p = p
        trim = f . f where f = reverse . dropWhile isSpace
        userAgent = concat [ "leksah-server/", display Paths_leksah_server.version
                           , " (", display buildOS, "; ", display buildArch, ")"
                           ]
        writeMerged packageDescrS packageDescrHi fpSource = do
            let mergedPackageDescr = mergePackageDescrs packageDescrHi packageDescrS
            liftIO $ writeExtractedPackage writeAscii mergedPackageDescr
            liftIO $ writePackagePath (dropFileName fpSource) packageName
        runCabalConfigure fpSource = do
            let dirPath         = dropFileName fpSource
                packageName'    = takeBaseName fpSource
                flagsFor "base" = do
                    libDir <- getSysLibDir mbProject Nothing
                    return $ ["-finteger-gmp", "-finteger-gmp2"] ++ maybeToList ((\l -> T.pack $ "--configure-option=CFLAGS=-I" <> l </> "include") <$> libDir)
                flagsFor ('g':'i':'-':_) = return ["-f-enable-overloading", "-f-overloaded-methods", "-f-overloaded-properties", "-f-overloaded-signals"]
                flagsFor _      = return []
            setCurrentDirectory dirPath
            E.catch (do _ <- runTool' "cabal" ["clean"] Nothing Nothing
                        debugM "leksah" $ "fpSource = " <> show fpSource
                        flags <- flagsFor packageName'
                        _ <- runProjectTool mbProject "cabal" ("configure":flags ++ map (("--package-db="<>) .T.pack) dbs) Nothing Nothing
                        return ())
                    (\ (_e :: E.SomeException) -> do
                        debugM "leksah-server" "Can't configure"
                        return ())

collectPackageNix :: Prefs -> PackageConfig -> Maybe FilePath -> IO (Maybe PackageCollectStats)
collectPackageNix _ _ Nothing = return Nothing
collectPackageNix prefs packageConfig (Just project) = do
    debugM "leksah-server" "collectPackageNix"
    let nixFile = takeDirectory project </> "default.nix"
    doesFileExist nixFile >>= \case
        True -> do
            collectorPath <- getCollectorPath
            leksahMetadataNix <- (</> "data/leksah-metadata.nix") <$> getDataDir
            (nixOuput, _) <- runTool' "nix-build" ["-E", T.pack $
                                   "let ghc=(let fn = import ./.; in if builtins.isFunction fn then fn {} else fn).ghc; "
                                <> "in import " <> leksahMetadataNix <> " { inherit ghc; "
                                <> "pkg=ghc." <> display (pkgName pid) <> ";}"
                                ] (Just $ takeDirectory project) Nothing
            case reverse nixOuput of
                (ToolExit ExitSuccess:ToolOutput lineMaybeQuoted:_) -> do
                    let line = T.unpack $ removeQuotes lineMaybeQuoted
                        metadataFile = line </> "share/leksah/metadata"
                                            </> packageName' <.> leksahMetadataSystemFileExtension
                        pkgSource = line </> "share/leksah/packageSource"
                    doesFileExist metadataFile >>= \case
                        True -> do
                            BS.readFile metadataFile >>= BS.writeFile (
                                collectorPath </> packageName' <.> leksahMetadataSystemFileExtension)
                            doesDirectoryExist pkgSource >>= \case
                                True -> do
                                    unpackDir <- getUnpackDirectory prefs
                                    case unpackDir of
                                        Nothing -> return $ Just stat {packageString = "metadata from nix (no unpack dir)"}
                                        Just fpUnpack -> do
                                            createDirectoryIfMissing True fpUnpack
                                            installDirectoryContents normal pkgSource (fpUnpack </> packageName')
                                            writePackagePath (fpUnpack </> packageName' <> "/") packageName
                                            return $ Just stat {packageString = "metadata and source from nix"}
                                False -> return $ Just stat {packageString = "metadata from nix"}
                        False -> return Nothing
                _ -> return Nothing
        False -> return Nothing
  where
    pid = packId $ getThisPackage packageConfig
    packageName = packageIdentifierToString pid
    packageName' = T.unpack packageName
    stat = PackageCollectStats packageName Nothing False False Nothing
    removeQuotes s = fromMaybe s $ T.stripPrefix "\"" s >>= T.stripSuffix "\""


collectPackageOnly :: PackageConfig -> [FilePath] -> FilePath -> FilePath -> IO ()
collectPackageOnly packageConfig dbs fpSource outputFile = do
        debugM "leksah-server" $ "Build " <> T.unpack packageName <> " in " <> fpSource <> " out " <> outputFile
        build
    where
        pid = packId $ getThisPackage packageConfig
        packageName = packageIdentifierToString pid
        build :: IO ()
        build =
            collectPackageFromHI Nothing packageConfig dbs >>= \case
                Nothing -> return ()
                Just packageDescrHi -> do
                    mbPackageDescrPair <- packageFromSource Nothing dbs fpSource packageConfig
                    case mbPackageDescrPair of
                        (Just packageDescrS, _) ->
                            encodeFileSer outputFile (metadataVersion, mergePackageDescrs packageDescrHi packageDescrS)
                        (Nothing, _) ->
                            encodeFileSer outputFile (metadataVersion, packageDescrHi)

writeExtractedPackage :: MonadIO m => Bool -> PackageDescr -> m ()
writeExtractedPackage writeAscii pd = do
    collectorPath   <- liftIO getCollectorPath
    let filePath    =  collectorPath </> T.unpack (packageIdentifierToString $ pdPackage pd) <.>
                            leksahMetadataSystemFileExtension
    if writeAscii
        then liftIO $ writeFile (filePath ++ "dpg") (show pd)
        else liftIO $ encodeFileSer filePath (metadataVersion, pd)

writePackagePath :: MonadIO m => FilePath -> Text -> m ()
writePackagePath fp packageName = do
    collectorPath   <- liftIO getCollectorPath
    let filePath    =  collectorPath </> T.unpack packageName <.> leksahMetadataPathFileExtension
    liftIO $ writeFile filePath fp

--------------Merging of .hi and .hs parsing / parsing and typechecking results

mergePackageDescrs :: PackageDescr -> PackageDescr -> PackageDescr
mergePackageDescrs packageDescrHI packageDescrS = PackageDescr {
        pdPackage           =   pdPackage packageDescrHI
    ,   pdMbSourcePath      =   pdMbSourcePath packageDescrS
    ,   pdModules           =   mergeModuleDescrs (pdModules packageDescrHI) (pdModules packageDescrS)
    ,   pdBuildDepends      =   pdBuildDepends packageDescrHI}

mergeModuleDescrs :: [ModuleDescr] -> [ModuleDescr] -> [ModuleDescr]
mergeModuleDescrs hiList srcList =  map mergeIt allNames
    where
        mergeIt :: String -> ModuleDescr
        mergeIt str = case (Map.lookup str hiDict, Map.lookup str srcDict) of
                        (Just mdhi, Nothing) -> mdhi
                        (Nothing, Just mdsrc) -> mdsrc
                        (Just mdhi, Just mdsrc) -> mergeModuleDescr mdhi mdsrc
                        (Nothing, Nothing) -> error "Collector>>mergeModuleDescrs: impossible"
        allNames = nub $ Map.keys hiDict ++  Map.keys srcDict
        hiDict = Map.fromList $ zip (map (display . modu . mdModuleId) hiList) hiList
        srcDict = Map.fromList $ zip (map (display . modu . mdModuleId) srcList) srcList

mergeModuleDescr :: ModuleDescr -> ModuleDescr -> ModuleDescr
mergeModuleDescr hiDescr srcDescr = ModuleDescr {
        mdModuleId          = mdModuleId hiDescr
    ,   mdMbSourcePath      = mdMbSourcePath srcDescr
    ,   mdReferences        = mdReferences hiDescr
    ,   mdIdDescriptions    = mergeDescrs (mdIdDescriptions hiDescr) (mdIdDescriptions srcDescr)}

mergeDescrs :: [Descr] -> [Descr] -> [Descr]
mergeDescrs hiList srcList =  concatMap mergeIt allNames
    where
        mergeIt :: Text -> [Descr]
        mergeIt pm = case (Map.lookup pm hiDict, Map.lookup pm srcDict) of
                        (Just mdhi, Nothing) -> mdhi
                        (Nothing, Just mdsrc) -> mdsrc
                        (Just mdhi, Just mdsrc) -> map (uncurry mergeDescr) $ makePairs mdhi mdsrc
                        (Nothing, Nothing) -> error "Collector>>mergeModuleDescrs: impossible"
        allNames   = nub $ Map.keys hiDict ++  Map.keys srcDict
        hiDict     = Map.fromListWith (++) $ zip (map dscName hiList) (map (: []) hiList)
        srcDict    = Map.fromListWith (++) $ zip (map dscName srcList)(map (: []) srcList)

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
    ,   dscMbTypeStr'   = dscMbTypeStr' rdsrc
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

--mergeTypeHint :: Maybe TypeDescr -> Maybe TypeDescr -> Maybe TypeDescr
--mergeTypeHint Nothing Nothing         = Nothing
--mergeTypeHint Nothing jtd             = jtd
--mergeTypeHint jtd Nothing             = jtd
--mergeTypeHint (Just tdhi) (Just tdhs) = Just (mergeTypeDescr tdhi tdhs)

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
        mergeIt :: Text -> SimpleDescr
        mergeIt pm = fromMaybe (error "Collector>>mergeSimpleDescrs: impossible")
                       (mergeMbDescr (Map.lookup pm hiDict) (Map.lookup pm srcDict))
        allNames   = nub $ Map.keys hiDict ++  Map.keys srcDict
        hiDict     = Map.fromList $ zip (map sdName hiList) hiList
        srcDict    = Map.fromList $ zip (map sdName srcList) srcList

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




