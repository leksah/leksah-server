{-# OPTIONS_GHC -XScopedTypeVariables -XBangPatterns #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Metainfo.SourceCollectorH
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

module IDE.Metainfo.SourceCollectorH (
    collectPackageFromSource
,   packageFromSource
,   interfaceToModuleDescr
,   PackageCollectStats(..)
) where

import IDE.Core.CTypes
       (PackageDescr(..), TypeDescr(..), RealDescr(..), Descr(..),
        ModuleDescr(..), PackModule(..), SimpleDescr(..), packageIdentifierToString)
import Haddock.Types
       (ExportItem(..), DeclInfo(..),
        Interface(..))
import Distribution.Text (display, simpleParse)
import InstEnv (Instance(..))
import MyMissing
import Data.Map (Map(..))
import qualified Data.Map as Map (empty)

import Outputable (ppr, showSDocUnqual, OutputableBndr)
import Data.List (nub)
import qualified Data.ByteString.Char8 as BS (pack)
import IDE.Metainfo.WorkspaceCollector
       (uncommentData, toComment, srcSpanToLocation, printHsDoc,
       attachComments, uncommentDecl)
import Name (getOccString,getSrcSpan)
import PackageConfig (PackageConfig(..))
import Haddock.Interface (createInterfaces)
import Distribution.Verbosity (verbose)
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.InstalledPackageInfo (depends)
import IDE.StrippedPrefs (Prefs(..))
import IDE.Metainfo.SourceDB (sourceForPackage, getSourcesMap)
import MonadUtils (liftIO)
import Debug.Trace (trace)
import System.Directory (setCurrentDirectory, doesDirectoryExist,createDirectory,canonicalizePath)
import System.FilePath (dropFileName,(</>),(<.>))
import Data.Maybe(mapMaybe)
import IDE.Utils.GHCUtils (inGhcIO)
import qualified Control.Exception as NewException (SomeException, catch)
import IDE.Utils.Tool
import Control.Monad (unless)
import IDE.Utils.FileUtils(figureOutHaddockOpts)
import Distribution.Package(PackageIdentifier)
import GHC hiding(Id,Failed,Succeeded,ModuleName)
import System.Log.Logger (warningM)

#if MIN_VERSION_Cabal(1,8,0)
getThisPackage    =   IPI.sourcePackageId
#else
getThisPackage    =   IPI.package
#endif

data PackageCollectStats = PackageCollectStats {
    packageString       :: String,
    modulesTotal        :: Maybe Int,
    withSource          :: Bool,
    retrieved           :: Bool,
    mbError             :: Maybe String}

-- Hell

collectPackageFromSource :: Prefs -> PackageConfig -> IO (Maybe PackageDescr, PackageCollectStats, Maybe FilePath)
collectPackageFromSource prefs packageConfig = trace ("collectPackageFromSource " ++ display (getThisPackage packageConfig))
    $ do
    sourceMap <- liftIO $ getSourcesMap prefs
    case sourceForPackage (getThisPackage packageConfig) sourceMap of
        Just fp -> do
            runTool' "cabal" (["configure","--user"]) Nothing
            packageFromSource fp packageConfig
        Nothing ->
            case unpackDirectory prefs of
                Nothing -> return (Nothing, PackageCollectStats packageName Nothing False False
                                (Just ("No source found. Prefs don't allow for retreiving")), Nothing)
                Just fp -> do
                    exists <- doesDirectoryExist fp
                    unless exists $ createDirectory fp
                    setCurrentDirectory fp
                    (output, pid) <- runTool' "cabal" (["unpack",packageName]) Nothing
                    success <- doesDirectoryExist (fp </> packageName)
                    if not success
                        then return (Nothing, PackageCollectStats packageName Nothing False False
                                (Just ("Failed to download and unpack source")), Nothing)
                        else do
                            setCurrentDirectory (fp </> packageName)
                            output <- runTool' "cabal" (["configure","--user"]) Nothing
                            packageFromSource (fp </> packageName </> packageName <.> "cabal")
                                        packageConfig
    where
        packageName = packageIdentifierToString (getThisPackage packageConfig)

packageFromSource :: FilePath -> PackageConfig -> IO (Maybe PackageDescr, PackageCollectStats, Maybe FilePath)
packageFromSource cabalPath packageConfig = trace ("packageFromSource " ++ cabalPath)
    $ do
    setCurrentDirectory dirPath
    ghcFlags <- figureOutHaddockOpts
    trace ("ghcFlags:  " ++ show ghcFlags)
        NewException.catch (inner ghcFlags) handler
    where
        handler' (e :: NewException.SomeException) =
            trace "would block" $ return ([])
        handler (e :: NewException.SomeException) = do
            warningM "leksah-server" ("Ghc failed to process: " ++ show e)
            return (Nothing, PackageCollectStats packageName Nothing False False
                                            (Just ("Ghc failed to process: " ++ show e)), Just dirPath)
        inner ghcFlags = trace ("before  inGhcIO ") $
                                inGhcIO ghcFlags [Opt_Haddock, Opt_Cpp] $ \ flags -> do
            (interfaces,_) <- createInterfaces verbose (exportedMods ++ hiddenMods) [] []
            liftIO $ print (length interfaces)
            let mods = map (interfaceToModuleDescr dirPath (getThisPackage packageConfig)) interfaces
            sp <- liftIO $ canonicalizePath dirPath
            let pd = PackageDescr {
                    pdPackage           =   getThisPackage packageConfig
                ,   pdModules           =   mods
                ,   pdBuildDepends      =   depends packageConfig
                ,   pdMbSourcePath      =   Just sp}
            let stat = PackageCollectStats packageName (Just (length mods)) True False Nothing
            liftIO $ return (Just pd, stat, Just dirPath)
        exportedMods = map moduleNameString $ IPI.exposedModules packageConfig
        hiddenMods   = map moduleNameString $ IPI.hiddenModules packageConfig
        dirPath      = dropFileName cabalPath
        packageName  = packageIdentifierToString (getThisPackage packageConfig)

-- Heaven

interfaceToModuleDescr :: FilePath -> PackageIdentifier -> Interface -> ModuleDescr
interfaceToModuleDescr dirPath pid interface = trace ("interfaceToModuleDescr " ++ show modName ++ " " ++ show filepath)
    ModuleDescr {
        mdModuleId          =   PM pid modName
    ,   mdMbSourcePath      =   Just filepath
    ,   mdReferences        =   imports
    ,   mdIdDescriptions    =   descrs}
    where
        filepath   = ifaceOrigFilename interface
        modName    = forceJust ((simpleParse . moduleNameString . moduleName . ifaceMod) interface)
                        "Can't parse module name"
        descrs     = extractDescrs (PM pid modName)
                        (ifaceDeclMap interface) (ifaceExportItems interface)
                        (ifaceInstances interface) (ifaceLocals interface)
        imports    = Map.empty --TODO

extractDescrs :: PackModule -> Map Name DeclInfo -> [ExportItem Name] -> [Instance] -> [Name] -> [Descr]
extractDescrs pm ifaceDeclMap ifaceExportItems ifaceInstances ifaceLocals =
	transformToDescrs pm exportedDeclInfo ++ map (toDescrInst pm) ifaceInstances
    where
        transformed                                    = transformToDescrs pm exportedDeclInfo
                                                            ++ map (toDescrInst pm) ifaceInstances
        exportedDeclInfo                               =  mapMaybe toDeclInfo  ifaceExportItems
        toDeclInfo (ExportDecl decl mbDoc subDocs _)   = Just(decl,mbDoc,subDocs)
        toDeclInfo (ExportNoDecl _ _)                  = Nothing
        toDeclInfo (ExportGroup _ _ _)                 = Nothing
        toDeclInfo (ExportDoc _)                       = Nothing
        toDeclInfo (ExportModule _)                    = Nothing


-- transformToDescrs :: PackModule -> [(Decl, Maybe Doc, [(Name, Maybe Doc)])] -> [Descr]
transformToDescrs pm = concatMap transformToDescr
    where
--    transformToDescr :: (Decl, Maybe Doc, [(Name, Maybe Doc)]) -> [Descr]
    transformToDescr ((L loc (SigD (TypeSig name typ))), mbComment,subCommentList) =
        [Real $ RealDescr {
        dscName'        =   getOccString (unLoc name)
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual $ppr typ))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   Just (srcSpanToLocation loc)
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   VariableDescr
    ,   dscExported'    =   True}]

    transformToDescr ((L loc (SigD _)), mbComment,subCommentList) = []
    transformToDescr ((L loc (TyClD typ@(TySynonym lid _ _ _ ))), mbComment,subCommentList) =
        [Real $ RealDescr {
        dscName'        =   getOccString (unLoc lid)
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual $ppr typ))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   Just (srcSpanToLocation loc)
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]

    transformToDescr ((L loc (TyClD typ@(TyData DataType _ tcdLName _ _ _ lConDecl tcdDerivs))), mbComment,subCommentList) =
        [Real $ RealDescr {
        dscName'        =   name
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual $ppr (uncommentData typ)))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   Just (srcSpanToLocation loc)
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   DataDescr constructors fields
    ,   dscExported'    =   True}]
            ++ derivings tcdDerivs
        where
        constructors    =   map extractConstructor lConDecl
        fields          =   nub $ concatMap extractRecordFields lConDecl
        name            =   getOccString (unLoc tcdLName)
        derivings Nothing = []
        derivings (Just l) = []

    transformToDescr ((L loc (TyClD typ@(TyData NewType _ tcdLName _ _ _ lConDecl tcdDerivs))), mbComment,subCommentList) =
        [Real $ RealDescr {
        dscName'        =   name
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual $ppr (uncommentData typ)))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   Just (srcSpanToLocation loc)
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   NewtypeDescr constructor mbField
    ,   dscExported'    =   True}]
        ++ derivings tcdDerivs
        where
        constructor     =   forceHead (map extractConstructor lConDecl)
                                "WorkspaceCollector>>transformToDescr: no constructor for newtype"
        mbField         =   case concatMap extractRecordFields lConDecl of
                                [] -> Nothing
                                a:_ -> Just a
        name            =   getOccString (unLoc tcdLName)
        derivings Nothing = []
        derivings (Just l) = []

    transformToDescr ((L loc (TyClD cl@(ClassDecl _ tcdLName _ _ tcdSigs _ _ docs))), mbComment,subCommentList) =
        [Real $ RealDescr {
        dscName'        =   getOccString (unLoc tcdLName)
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual $ppr cl{tcdMeths = emptyLHsBinds}))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   Just (srcSpanToLocation loc)
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   ClassDescr super methods
    ,   dscExported'    =   True    }]
        where
        methods         =   extractMethods tcdSigs docs
        super           =   []

    transformToDescr (_, mbComment,sigList) = []

toDescrInst :: PackModule -> Instance -> Descr
toDescrInst pm inst@(Instance is_cls is_tcs is_tvs is_tys is_dfun is_flag) =
        Real $ RealDescr {
        dscName'        =   getOccString is_cls
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual $ppr inst))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   Just (srcSpanToLocation (getSrcSpan inst))
    ,   dscMbComment'   =   Nothing
    ,   dscTypeHint'    =   InstanceDescr (map (showSDocUnqual . ppr) is_tys)
    ,   dscExported'    =   True}

#if MIN_VERSION_Cabal(1,8,0)
type MaybeDocStrings = [HsDocString]


extractMethods :: (OutputableBndr alpha, NamedThing alpha) => [LSig alpha] -> [LDocDecl ] -> [SimpleDescr]
extractMethods sigs docs =
    let pairs = attachComments sigs docs
    in mapMaybe extractMethod pairs

extractMethod :: (OutputableBndr alpha, NamedThing alpha) => (LHsDecl alpha, MaybeDocStrings) -> Maybe SimpleDescr
extractMethod ((L loc (SigD ts@(TypeSig name typ))), mbDoc) =
    Just $ SimpleDescr
        (getOccString (unLoc name))
        (Just (BS.pack (showSDocUnqual $ ppr ts)))
        (Just (srcSpanToLocation loc))
        (toComment mbDoc [])
extractMethod (_, mbDoc) = Nothing

extractConstructor decl@(L loc (ConDecl name _ _ _ _ _ doc _)) =
    SimpleDescr
        (getOccString (unLoc name))
        (Just (BS.pack (showSDocUnqual $ppr (uncommentDecl decl))))
        (Just (srcSpanToLocation loc))
        (case doc of
            Nothing -> Nothing
            Just (L _ d) -> Just (BS.pack (printHsDoc d)))


extractRecordFields (L _ decl@(ConDecl _ _ _ _ (RecCon flds) _ _ _)) =
    map extractRecordFields' flds
    where
    extractRecordFields' field@(ConDeclField (L loc name) typ doc) =
        SimpleDescr
            (getOccString name)
            (Just (BS.pack (showSDocUnqual $ ppr typ)))
            (Just (srcSpanToLocation loc))
            (case doc of
                Nothing -> Nothing
                Just (L _ d) -> Just (BS.pack (printHsDoc d)))
            True
extractRecordFields _ = []

#else
extractMethods :: (OutputableBndr alpha, NamedThing alpha) => [LSig alpha] -> [LDocDecl alpha] -> [SimpleDescr]
extractMethods sigs docs =
    let pairs = attachComments sigs docs
    in mapMaybe extractMethod pairs

extractMethod :: (OutputableBndr alpha, NamedThing alpha) => (LHsDecl alpha, Maybe (HsDoc alpha)) -> Maybe SimpleDescr
extractMethod ((L loc (SigD ts@(TypeSig name typ))), mbDoc) =
    Just $ SimpleDescr
        (getOccString (unLoc name))
        (Just (BS.pack (showSDocUnqual $ ppr ts)))
        (Just (srcSpanToLocation loc))
        (toComment mbDoc [])
        True
extractMethod (_, mbDoc) = Nothing

extractConstructor decl@(L loc (ConDecl name _ _ _ _ _ doc)) =
    SimpleDescr
        (getOccString (unLoc name))
        (Just (BS.pack (showSDocUnqual $ppr (uncommentDecl decl))))
        (Just (srcSpanToLocation loc))
        (case doc of
            Nothing -> Nothing
            Just (L _ d) -> Just (BS.pack (printHsDoc d)))
        True


extractRecordFields (L _ decl@(ConDecl _ _ _ _ (RecCon flds) _ _)) =
    map extractRecordFields' flds
    where
    extractRecordFields' field@(ConDeclField (L loc name) typ doc) =
        SimpleDescr
            (getOccString name)
            (Just (BS.pack (showSDocUnqual $ ppr typ)))
            (Just (srcSpanToLocation loc))
            (case doc of
                Nothing -> Nothing
                Just (L _ d) -> Just (BS.pack (printHsDoc d)))
            True
extractRecordFields _ = []
#endif


