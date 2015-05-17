{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
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
       (getThisPackage, SimpleDescr(..), TypeDescr(..),
        ReexportedDescr(..), Descr(..), RealDescr(..), dscTypeHint,
        descrType, dscName, Descr, ModuleDescr(..), PackModule(..),
        PackageDescr(..), metadataVersion, leksahVersion,
        packageIdentifierToString, packId)
import IDE.Utils.FileUtils (getCollectorPath)
import System.Directory (doesDirectoryExist, setCurrentDirectory)
import IDE.Utils.Utils
       (leksahMetadataPathFileExtension,
        leksahMetadataSystemFileExtension)
import System.FilePath (dropFileName, (<.>), (</>))
import Data.Binary.Shared (encodeFileSer)
import qualified Data.Map as Map
       (fromListWith, fromList, keys, lookup)
import Data.List (delete, nub)
import Distribution.Text (display)
import Control.Monad.IO.Class (MonadIO, MonadIO(..))
import qualified Control.Exception as E (SomeException, catch)
import IDE.Utils.Tool (runTool')
import Data.Monoid ((<>))
import qualified Data.Text as T (unpack)
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
import Control.Monad (unless)

collectPackage :: Bool -> Prefs -> Int -> (PackageConfig,Int) -> IO PackageCollectStats
collectPackage writeAscii prefs numPackages (packageConfig, packageIndex) = do
    infoM "leksah-server" ("update_toolbar " ++ show
        ((fromIntegral packageIndex / fromIntegral numPackages) :: Double))
    let packageName = packageIdentifierToString (packId $ getThisPackage packageConfig)
    let stat = PackageCollectStats packageName Nothing False False Nothing
    eitherStrFp    <- findSourceForPackage prefs (packId $ getThisPackage packageConfig)
    case eitherStrFp of
        Left message -> do
            debugM "leksah-server" . T.unpack $ message <> " : " <> packageName
            packageDescrHi <- collectPackageFromHI packageConfig
            writeExtractedPackage False packageDescrHi
            return stat {packageString = message, modulesTotal = Just (length (pdModules packageDescrHi))}
        Right fpSource ->
            case retrieveStrategy prefs of
                RetrieveThenBuild -> do
                    success <- retrieve packageName
                    if success
                        then do
                            debugM "leksah-server" . T.unpack $ "collectPackage: retreived = " <> packageName
                            liftIO $ writePackagePath (dropFileName fpSource) packageName
                            return (stat {withSource=True, retrieved= True, mbError=Nothing})
                        else do
                            debugM "leksah-server" . T.unpack $ "collectPackage: Can't retreive = " <> packageName
                            runCabalConfigure fpSource
                            mbPackageDescrPair <- packageFromSource fpSource packageConfig
                            case mbPackageDescrPair of
                                (Just packageDescrS, bstat) ->  do
                                    writePackageDesc packageDescrS fpSource packageName
                                    return bstat{modulesTotal = Just (length (pdModules packageDescrS))}
                                (Nothing,bstat) ->  do
                                    packageDescrHi <- collectPackageFromHI packageConfig
                                    writeExtractedPackage False packageDescrHi
                                    return bstat{modulesTotal = Just (length (pdModules packageDescrHi))}
                BuildThenRetrieve -> do
                        debugM "leksah-server" $ "Build (then retrieve) " <> T.unpack packageName <> " in " <> fpSource
                        runCabalConfigure fpSource
                        mbPackageDescrPair <- packageFromSource fpSource packageConfig
                        case mbPackageDescrPair of
                            (Just packageDescrS,bstat) ->  do
                                writePackageDesc packageDescrS fpSource packageName
                                return bstat{modulesTotal = Just (length (pdModules packageDescrS))}
                            (Nothing,bstat) ->  do
                                success  <- retrieve packageName
                                if success
                                    then do
                                        debugM "leksah-server" . T.unpack $ "collectPackage: retreived = " <> packageName
                                        liftIO $ writePackagePath (dropFileName fpSource) packageName
                                        return (stat {withSource=True, retrieved= True, mbError=Nothing})
                                    else do
                                        packageDescrHi <- collectPackageFromHI packageConfig
                                        writeExtractedPackage False packageDescrHi
                                        return bstat{modulesTotal = Just (length (pdModules packageDescrHi))}
                NeverRetrieve -> do
                        debugM "leksah-server" $ "Build " <> T.unpack packageName <> " in " <> fpSource
                        runCabalConfigure fpSource
                        mbPackageDescrPair <- packageFromSource fpSource packageConfig
                        case mbPackageDescrPair of
                                (Just packageDescrS,bstat) ->  do
                                    writePackageDesc packageDescrS fpSource packageName
                                    return bstat{modulesTotal = Just (length (pdModules packageDescrS))}
                                (Nothing,bstat) ->  do
                                    packageDescrHi <- collectPackageFromHI packageConfig
                                    writeExtractedPackage False packageDescrHi
                                    return bstat{modulesTotal = Just (length (pdModules packageDescrHi))}
    where
        retrieve :: Text -> IO Bool
        retrieve packString = do
            collectorPath   <- liftIO $ getCollectorPath
            setCurrentDirectory collectorPath
            let fullUrl  = T.unpack (retrieveURL prefs) <> "/metadata-" <> leksahVersion <> "/" <> T.unpack packString <> leksahMetadataSystemFileExtension
                filePath = collectorPath </> T.unpack packString <.> leksahMetadataSystemFileExtension

            case parseURI fullUrl of
                Nothing -> do
                    errorM "leksah-server" $ "collectPackage: invalid URI = " <> fullUrl
                    return False
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
                            return True
                        else return False

        trimProxyUri (Proxy uri auth) = Proxy (trim uri) auth
        trimProxyUri p = p
        filterEmptyProxy (Proxy "" _) = NoProxy
        filterEmptyProxy p = p
        trim = f . f where f = reverse . dropWhile isSpace
        userAgent = concat [ "leksah-server/", display Paths_leksah_server.version
                           , " (", display buildOS, "; ", display buildArch, ")"
                           ]
        writePackageDesc packageDescr fpSource packageName = do
            liftIO $ writeExtractedPackage writeAscii packageDescr
            liftIO $ writePackagePath (dropFileName fpSource) packageName
        runCabalConfigure fpSource = do
            let dirPath      = dropFileName fpSource
            distExists <- doesDirectoryExist $ dirPath </> "dist"
            unless distExists $ do
                setCurrentDirectory dirPath
                E.catch (do runTool' "cabal" ["clean"] Nothing
                            runTool' "cabal" ["configure","--user"] Nothing
                            return ())
                        (\ (_e :: E.SomeException) -> do
                            debugM "leksah-server" "Can't configure"
                            return ())

writeExtractedPackage :: MonadIO m => Bool -> PackageDescr -> m ()
writeExtractedPackage writeAscii pd = do
    collectorPath   <- liftIO $ getCollectorPath
    let filePath    =  collectorPath </> T.unpack (packageIdentifierToString $ pdPackage pd) <.>
                            leksahMetadataSystemFileExtension
    if writeAscii
        then liftIO $ writeFile (filePath ++ "dpg") (show pd)
        else liftIO $ encodeFileSer filePath (metadataVersion, pd)

writePackagePath :: MonadIO m => FilePath -> Text -> m ()
writePackagePath fp packageName = do
    collectorPath   <- liftIO $ getCollectorPath
    let filePath    =  collectorPath </> T.unpack packageName <.> leksahMetadataPathFileExtension
    liftIO $ writeFile filePath fp

