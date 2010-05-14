{-# OPTIONS_GHC -XScopedTypeVariables -fno-warn-type-defaults #-}
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

import IDE.StrippedPrefs (RetrieveStrategy(..), Prefs(..))
import PackageConfig (PackageConfig)
import IDE.Metainfo.SourceCollectorH
       (findSourceForPackage, packageFromSource, PackageCollectStats(..))
import System.Log.Logger (debugM, infoM)
import IDE.Metainfo.InterfaceCollector (collectPackageFromHI)
import IDE.Core.CTypes
       (getThisPackage, SimpleDescr(..), TypeDescr(..),
        ReexportedDescr(..), Descr(..), RealDescr(..), dscTypeHint,
        descrType, dscName, Descr, ModuleDescr(..), PackModule(..),
        PackageDescr(..), metadataVersion, leksahVersion,
        packageIdentifierToString)
import Control.Monad.Trans (MonadIO, MonadIO(..))
import IDE.Utils.FileUtils (getCollectorPath)
import System.Directory (doesFileExist, setCurrentDirectory)
import IDE.Utils.Utils
       (leksahMetadataPathFileExtension,
        leksahMetadataSystemFileExtension)
import IDE.System.Process (system)
import System.FilePath (dropFileName, (<.>), (</>))
import Data.Binary.Shared (encodeFileSer)
import qualified Data.Map as Map
       (fromListWith, fromList, keys, lookup)
import Data.List (delete, nub)
import Distribution.Text (display)
import Control.Exception (SomeException,catch)
import GHC.IO.Exception (ExitCode(..))
import Prelude hiding(catch)

collectPackage :: Bool -> Prefs -> Int -> (PackageConfig,Int) -> IO PackageCollectStats
collectPackage writeAscii prefs numPackages (packageConfig, packageIndex) = do
    infoM "leksah-server" ("update_toolbar " ++ show
        ((fromIntegral packageIndex / fromIntegral numPackages) :: Double))
    let packageName = packageIdentifierToString (getThisPackage packageConfig)
    let stat = PackageCollectStats packageName Nothing False False Nothing
    eitherStrFp    <- findSourceForPackage prefs packageConfig
    case eitherStrFp of
        Left message -> do
            packageDescrHi <- collectPackageFromHI packageConfig
            writeExtractedPackage False packageDescrHi
            return stat {packageString = message, modulesTotal = Just (length (pdModules packageDescrHi))}
        Right fpSource ->
            case retrieveStrategy prefs of
                RetrieveThenBuild -> do
                    success <- retrieve packageName
                    if success
                        then do
                            debugM "leksah-server" $ "collectPackage: retreived = " ++ packageName
                            liftIO $ writePackagePath (dropFileName fpSource) packageName
                            return (stat {withSource=True, retrieved= True, mbError=Nothing})
                        else do
                            debugM "leksah-server" $ "collectPackage: Can't retreive = " ++ packageName
                            packageDescrHi <- collectPackageFromHI packageConfig
                            mbPackageDescrPair <- packageFromSource fpSource packageConfig
                            case mbPackageDescrPair of
                                (Just packageDescrS, bstat) ->  do
                                    writeMerged packageDescrS packageDescrHi fpSource packageName
                                    return bstat{modulesTotal = Just (length (pdModules packageDescrHi))}
                                (Nothing,bstat) ->  do
                                    writeExtractedPackage False packageDescrHi
                                    return bstat{modulesTotal = Just (length (pdModules packageDescrHi))}
                BuildThenRetrieve -> do
                        mbPackageDescrPair <- packageFromSource fpSource packageConfig
                        case mbPackageDescrPair of
                            (Just packageDescrS,bstat) ->  do
                                packageDescrHi <- collectPackageFromHI packageConfig
                                writeMerged packageDescrS packageDescrHi fpSource packageName
                                return bstat{modulesTotal = Just (length (pdModules packageDescrHi))}
                            (Nothing,bstat) ->  do
                                success  <- retrieve packageName
                                if success
                                    then do
                                        debugM "leksah-server" $ "collectPackage: retreived = " ++ packageName
                                        liftIO $ writePackagePath (dropFileName fpSource) packageName
                                        return (stat {withSource=True, retrieved= True, mbError=Nothing})
                                    else do
                                        packageDescrHi <- collectPackageFromHI packageConfig
                                        writeExtractedPackage False packageDescrHi
                                        return bstat{modulesTotal = Just (length (pdModules packageDescrHi))}
                NeverRetrieve -> do
                        packageDescrHi <- collectPackageFromHI packageConfig
                        mbPackageDescrPair <- packageFromSource fpSource packageConfig
                        case mbPackageDescrPair of
                                (Just packageDescrS,bstat) ->  do
                                    writeMerged packageDescrS packageDescrHi fpSource packageName
                                    return bstat{modulesTotal = Just (length (pdModules packageDescrHi))}
                                (Nothing,bstat) ->  do
                                    writeExtractedPackage False packageDescrHi
                                    return bstat{modulesTotal = Just (length (pdModules packageDescrHi))}
    where
        retrieve :: String -> IO Bool
        retrieve packString = do
            collectorPath   <- liftIO $ getCollectorPath
            setCurrentDirectory collectorPath
            let fullUrl = retrieveURL prefs ++ "/metadata-" ++ leksahVersion ++ "/" ++ packString ++ leksahMetadataSystemFileExtension
            debugM "leksah-server" $ "collectPackage: before retreiving = " ++ fullUrl
            catch (system $ "wget " ++ fullUrl)
                (\(e :: SomeException) -> do
                    debugM "leksah-server" $ "collectPackage: Error when calling wget " ++ show e
                    return (ExitFailure 1))
            debugM "leksah-server" $ "collectPackage: after retreiving = " ++ packString -- ++ " result = " ++ res
            let filePath    =  collectorPath </> packString <.> leksahMetadataSystemFileExtension
            exist <- doesFileExist filePath
            return exist
        writeMerged packageDescrS packageDescrHi fpSource packageName = do
            let mergedPackageDescr = mergePackageDescrs packageDescrHi packageDescrS
            liftIO $ writeExtractedPackage writeAscii mergedPackageDescr
            liftIO $ writePackagePath (dropFileName fpSource) packageName

writeExtractedPackage :: MonadIO m => Bool -> PackageDescr -> m ()
writeExtractedPackage writeAscii pd = do
    collectorPath   <- liftIO $ getCollectorPath
    let filePath    =  collectorPath </> packageIdentifierToString (pdPackage pd) <.>
                            leksahMetadataSystemFileExtension
    if writeAscii
        then liftIO $ writeFile (filePath ++ "dpg") (show pd)
        else liftIO $ encodeFileSer filePath (metadataVersion, pd)

writePackagePath :: MonadIO m => FilePath -> String -> m ()
writePackagePath fp packageName = do
    collectorPath   <- liftIO $ getCollectorPath
    let filePath    =  collectorPath </> packageName <.> leksahMetadataPathFileExtension
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




