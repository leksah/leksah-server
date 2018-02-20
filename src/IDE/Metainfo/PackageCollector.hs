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

) where

import Control.Applicative
import Prelude
import IDE.StrippedPrefs (RetrieveStrategy(..), Prefs(..))
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
import IDE.Utils.FileUtils (getCollectorPath)
import System.Directory (setCurrentDirectory)
import IDE.Utils.Utils
       (leksahMetadataPathFileExtension,
        leksahMetadataSystemFileExtension)
import System.FilePath (dropFileName, takeBaseName, (<.>), (</>))
import Data.Binary.Shared (encodeFileSer)
import Distribution.Text (display)
import Control.Monad.IO.Class (MonadIO, MonadIO(..))
import qualified Control.Exception as E (SomeException, catch)
import IDE.Utils.Tool (runTool')
import Data.Monoid ((<>))
import qualified Data.Text as T (unpack, pack)
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
import qualified Data.ByteString as BS (writeFile, empty)
import qualified Paths_leksah_server (version)
import Distribution.System (buildArch, buildOS)
import qualified Data.Map as Map
       (fromListWith, fromList, keys, lookup)
import Data.List (delete, nub)

collectPackage :: Bool -> Prefs -> Int -> ((PackageConfig, [FilePath]), Int) -> IO PackageCollectStats
collectPackage writeAscii prefs numPackages ((packageConfig, dbs), packageIndex) = do
    infoM "leksah-server" ("update_toolbar " ++ show
        ((fromIntegral packageIndex / fromIntegral numPackages) :: Double))
    eitherStrFp    <- findSourceForPackage prefs pid
    case eitherStrFp of
        Left message -> do
            debugM "leksah-server" . T.unpack $ message <> " : " <> packageName
            packageDescrHi <- collectPackageFromHI packageConfig dbs
            writeExtractedPackage False packageDescrHi
            return stat {packageString = message, modulesTotal = Just (length (pdModules packageDescrHi))}
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
        build fpSource = do
            runCabalConfigure fpSource
            packageDescrHi <- collectPackageFromHI packageConfig dbs
            mbPackageDescrPair <- packageFromSource fpSource packageConfig
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
                flagsFor "base" = ["-finteger-gmp", "-finteger-gmp2"]
                flagsFor ('g':'i':'-':_) = ["-f-enable-overloading", "-f-overloaded-methods", "-f-overloaded-properties", "-f-overloaded-signals"]
                flagsFor _      = []
                flags           = flagsFor packageName'
            setCurrentDirectory dirPath
            E.catch (do _ <- runTool' "cabal" ["clean"] Nothing Nothing
                        debugM "leksah" $ "fpSource = " <> show fpSource
                        _ <- runTool' "cabal" ("configure":flags ++ map (("--package-db="<>) .T.pack) dbs) Nothing Nothing
                        return ())
                    (\ (_e :: E.SomeException) -> do
                        debugM "leksah-server" "Can't configure"
                        return ())

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
        mergeIt :: Text -> [Descr]
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




