{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Metainfo.SourceCollectorH
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

module IDE.Metainfo.SourceCollectorH (
--    collectPackageFromSource
    findSourceForPackage
,   packageFromSource
,   packageFromSource'
,   interfaceToModuleDescr
,   PackageCollectStats(..)
) where

import Prelude ()
import Prelude.Compat
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack, stripPrefix, stripSuffix)
import IDE.Core.CTypes
       (getThisPackage, PackageDescr(..), TypeDescr(..), RealDescr(..),
        Descr(..), ModuleDescr(..), PackModule(..), SimpleDescr(..),
        packageIdentifierToString, Location(..), RealDescr(..), PackageIdAndKey(..))


#if MIN_VERSION_haddock_library(1,10,0)
import Documentation.Haddock.Types (ModLink(..))
#endif
import Documentation.Haddock
import Distribution.Text (simpleParse, display)
import Data.Map (Map)
import qualified Data.Map as Map (empty, fromList, lookup)

import Data.List (nub, isSuffixOf)
import qualified Data.ByteString.Char8 as BS (pack)
import IDE.Metainfo.WorkspaceCollector
       (srcSpanToLocation, uncommentDecl, uncommentData, printHsDoc, sortByLoc)
import Distribution.Verbosity (verbose, normal)
#if MIN_VERSION_Cabal(3,17,0)
import Distribution.Verbosity (mkVerbosity, defaultVerbosityHandles)
#endif
#if MIN_VERSION_ghc(8,2,0)
#else
import GHC.PackageDb (exposedModules, hiddenModules, exposedName)
#endif
import Documentation.Haddock.Types (_doc)
import IDE.StrippedPrefs (getUnpackDirectory, Prefs(..))
import IDE.Metainfo.SourceDB (sourceForPackage, getSourcesMap)
import System.Directory (setCurrentDirectory, doesDirectoryExist, createDirectoryIfMissing, doesFileExist, makeAbsolute)
import System.FilePath ((<.>), dropFileName, (</>), splitDirectories, dropExtension)
import System.Exit (ExitCode(..))
import Data.Maybe(mapMaybe, listToMaybe, fromMaybe, catMaybes, isJust)
import IDE.Utils.GHCUtils (inGhcIO)
import qualified Control.Exception as NewException (SomeException, catch)
import IDE.Utils.Tool
import IDE.Utils.FileUtils (figureOutGhcOpts', myCanonicalizePath, getSysLibDir)
import Distribution.Package(PackageIdentifier, pkgName)
import Distribution.Simple.Utils (installDirectoryContents)
#if MIN_VERSION_ghc(9,0,0)
import GHC hiding(Id,Failed,Succeeded,HsDoc)
#else
import GHC hiding(Id,Failed,Succeeded,ModuleName)
#endif
#if MIN_VERSION_ghc(9,0,0)
import GHC.Utils.Outputable (Outputable, ppr, SDoc)
import GHC.Driver.Ppr (showSDoc)
import GHC.Types.Name (getOccString, getSrcSpan)
import GHC.Unit.Info (UnitInfo, unitExposedModules, unitHiddenModules)
import GHC.Core.InstEnv (ClsInst(..))
import GHC.Parser.Annotation (locA, getLocA)
import GHC.Hs.Doc (hsDocString)
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.Name.Occurrence (occNameString, isTcOcc)
import GHC.Unit.Module.Graph (mgModSummaries)
import Data.Foldable (toList)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
#endif
#if MIN_VERSION_ghc(9,14,0)
import GHC.Hs.Type (HsConDeclRecField(..), HsConDeclField(..))
#endif
#if MIN_VERSION_haddock_api(2,28,0)
import Haddock.Types (ExportD(..))
#endif
#if MIN_VERSION_ghc(9,6,0) && !MIN_VERSION_ghc(9,8,0)
import Haddock.Types (HaddockClsInst(..))
#endif
import Distribution.ModuleName (components)
import System.Log.Logger (warningM, debugM)
import Control.DeepSeq (deepseq)
import Data.ByteString.Char8 (ByteString)
import GHC.Show(showSpace)
import IDE.Utils.Project (ProjectKey, pjDir)

-- Cabal 3.17 (stable-haskell fork) split Verbosity into VerbosityFlags plus
-- output handles, so `normal` is now VerbosityFlags; rebuild a Verbosity for
-- the Cabal utils that still take one (installDirectoryContents below).
#if MIN_VERSION_Cabal(3,17,0)
normalVerbosity = mkVerbosity defaultVerbosityHandles normal
#else
normalVerbosity = normal
#endif

#if MIN_VERSION_ghc(8,2,0)
exposedName :: (ModuleName, Maybe Module) -> ModuleName
exposedName = fst
#endif

#if MIN_VERSION_ghc(9,0,0)
type PackageConfig = UnitInfo
showSDocUnqual :: DynFlags -> SDoc -> String
showSDocUnqual = showSDoc
#else
locA :: SrcSpan -> SrcSpan
locA = id
getLocA :: GenLocated SrcSpan e -> SrcSpan
getLocA = getLoc
#endif

#if !MIN_VERSION_ghc(8,4,0)
type GhcRn = Name
type family IdP p
type instance IdP GhcRn = Name
#endif

type HsDoc a = Doc a

type NDoc  = HsDoc Name

isEmptyDoc :: NDoc -> Bool
isEmptyDoc DocEmpty  = True
isEmptyDoc _         = False

#if MIN_VERSION_ghc(9,0,0)
type MyLDocDecl = LDocDecl GhcRn
#else
type MyLDocDecl = LDocDecl
#endif

show' :: Outputable alpha => DynFlags -> alpha  -> String
show' dflags = showSDoc dflags . ppr

data PackageCollectStats = PackageCollectStats {
    packageString       :: Text,
    modulesTotal        :: Maybe Int,
    withSource          :: Bool,
    retrieved           :: Bool,
    mbError             :: Maybe Text}

findSourceForPackage :: Prefs -> PackageIdentifier -> Maybe ProjectKey -> IO (Either Text FilePath)
findSourceForPackage prefs packageId mbProject = do
    debugM "leksah-server" $ "findSourceForPackage" <> display packageId <> " " <> show mbProject
    sourceMap <- liftIO $ getSourcesMap prefs
    case sourceForPackage packageId sourceMap of
        Just fpSource -> return (Right fpSource)
        Nothing -> do
            unpackDir <- getUnpackDirectory prefs
            case unpackDir of
                Nothing -> return (Left "No source found. Prefs don't allow for retreiving")
                Just fpUnpack -> do
                    createDirectoryIfMissing True fpUnpack
                    success <- maybe (return False) (copyNixSource packageId fpUnpack) mbProject >>= \case
                        True -> return True
                        False -> do
                            _ <- runTool' "cabal" ["unpack", packageName] (Just fpUnpack) Nothing
                            doesDirectoryExist (fpUnpack </> packageName')
                    return $ if success
                                then Right $ fpUnpack </> packageName' </> display (pkgName packageId) <.> "cabal"
                                else Left "Failed to download and unpack source"
    where
        packageName = packageIdentifierToString packageId
        packageName' = T.unpack packageName


copyNixSource :: PackageIdentifier -> FilePath -> ProjectKey -> IO Bool
copyNixSource packageId fpUnpack project = return False
copyNixSource packageId fpUnpack project = do
    debugM "leksah-server" "copyNixSource"
    let nixFile = pjDir project </> "default.nix"
    doesFileExist nixFile >>= \case
        True -> do
            (nixOuput, _) <- runTool' "nix-instantiate" [T.pack nixFile, "--eval", "-A", T.pack $ "ghc." <> display (pkgName packageId) <> ".src"] (Just $ pjDir project) Nothing
            case reverse nixOuput of
                (ToolExit ExitSuccess:ToolOutput lineMaybeQuoted:_) -> do
                    let line = removeQuotes lineMaybeQuoted
                        cabalFile = T.unpack line </> display (pkgName packageId) <.> "cabal"
                    doesFileExist cabalFile >>= \case
                        True -> installDirectoryContents normalVerbosity (T.unpack line) (fpUnpack </> packageName') >> return True
                        False -> do
                            debugM "leksah-server" $ "copyNixSource cabal file not found " <> cabalFile
                            return False
                _ -> return False
        False -> return False
    where
        packageName = packageIdentifierToString packageId
        packageName' = T.unpack packageName
        removeQuotes s = fromMaybe s $ T.stripPrefix "\"" s >>= T.stripSuffix "\""

packageFromSource :: Maybe ProjectKey -> [FilePath] -> FilePath -> PackageConfig -> IO (Maybe PackageDescr, PackageCollectStats)
packageFromSource mbProject dbs cabalPath packageConfig = do
    setCurrentDirectory (dropFileName cabalPath)
    ghcFlags <- figureOutGhcOpts' mbProject cabalPath
    debugM "leksah-server" ("ghcFlags:  " ++ show ghcFlags)
    NewException.catch (packageFromSource' ghcFlags dbs cabalPath packageConfig) handler
  where
    handler (e :: NewException.SomeException) = do
        warningM "leksah-server" ("Ghc failed to process: " ++ show e ++ " (" ++ cabalPath ++ ")")
        return (Nothing, PackageCollectStats packageName Nothing False False
                            (Just ("Ghc failed to process: " <> T.pack (show e) <> " (" <> T.pack cabalPath <> ")")))
    packageName  = packageIdentifierToString (packId $ getThisPackage packageConfig)

packageFromSource' :: [Text] -> [FilePath] -> FilePath -> PackageConfig -> IO (Maybe PackageDescr, PackageCollectStats)
packageFromSource' ghcFlags dbs cabalPath packageConfig = do
    debugM "leksah" $ "packageFromSource' " <> show ghcFlags <> " " <> show dbs <> " " <> show cabalPath
    getSysLibDir Nothing (Just VERSION_ghc) >>= \case
      Nothing -> do
        debugM "leksah-server" $ "packageFromSource' could not find system lib dir for GHC " <> VERSION_ghc <> " (used to build Leksah)"
        return (Nothing, PackageCollectStats packageName Nothing False False
                  (Just ("Ghc failed to process: could not find " <> VERSION_ghc <> " system lib dir (" <> T.pack cabalPath <> ")")))
      Just libDir -> inGhcIO libDir ghcFlags [Opt_Haddock] dbs $ \ dflags -> do
        (interfaces,_) <- processModules maxBound (exportedMods ++ hiddenMods) [] []
        liftIO $ print (length interfaces)
        let mods = map (interfaceToModuleDescr dflags dirPath (packId $ getThisPackage packageConfig)) interfaces
        -- Haddock can't build interfaces for modules that re-export across
        -- packages (it lacks the dependencies' .haddock interfaces), so their
        -- local declarations come back without source spans.  Recover those by
        -- parsing such modules directly (parsing needs no deps/typecheck).
        mods' <- fillSourceSpans dflags (packId $ getThisPackage packageConfig)
                                 (exportedMods ++ hiddenMods) mods
        sp <- liftIO $ myCanonicalizePath dirPath
        let pd = PackageDescr {
                pdPackage           =   packId (getThisPackage packageConfig)
            ,   pdModules           =   mods'
            ,   pdBuildDepends      =   [] -- TODO depends packageConfig
            ,   pdMbSourcePath      =   Just sp}
        let stat = PackageCollectStats packageName (Just (length mods')) True False Nothing
        liftIO $ deepseq pd $ return (Just pd, stat)
  where
#if MIN_VERSION_ghc(9,0,0)
    exportedMods = map (moduleNameString . exposedName) $ unitExposedModules packageConfig
    hiddenMods   = map moduleNameString $ unitHiddenModules packageConfig
#else
    exportedMods = map (moduleNameString . exposedName) $ exposedModules packageConfig
    hiddenMods   = map moduleNameString $ hiddenModules packageConfig
#endif
    dirPath      = dropFileName cabalPath
    packageName  = packageIdentifierToString (packId $ getThisPackage packageConfig)

-- | Recover source locations Haddock couldn't provide.  For any module whose
-- declarations Haddock left without a source span (typically modules that
-- re-export entities from other packages, which Haddock can't fully process
-- without the dependencies' .haddock interfaces), parse the module directly —
-- parsing needs no dependency resolution — and build descriptors with the real
-- spans of its top-level type signatures.  These merge with the .hi data (which
-- supplies types) in 'mergePackageDescrs'.
fillSourceSpans :: DynFlags -> PackageIdentifier -> [String] -> [ModuleDescr] -> Ghc [ModuleDescr]
#if MIN_VERSION_ghc(9,0,0)
fillSourceSpans dflags pid modNames mods = do
    summaries <- mgModSummaries <$> getModuleGraph
    let byName     = Map.fromList [ (display (modu (mdModuleId m)), m) | m <- mods ]
        realLoc (Real rd) = isJust (dscMbLocation' rd)
        realLoc _         = False
        located m  = not (maybe True null (mdMbSourcePath m)) && any realLoc (mdIdDescriptions m)
        summaryOf n = listToMaybe [ ms | ms <- summaries, moduleNameString (moduleName (ms_mod ms)) == n ]
    extras <- fmap catMaybes . forM modNames $ \n ->
        case Map.lookup n byName of
            Just m | located m -> return Nothing            -- Haddock already located it
            _ -> case summaryOf n of
                    Just ms -> Just <$> parseModuleDescr dflags pid ms
                    Nothing -> return Nothing
    let replaced = [ display (modu (mdModuleId m)) | m <- extras ]
    return ([ m | m <- mods, display (modu (mdModuleId m)) `notElem` replaced ] ++ extras)

parseModuleDescr :: DynFlags -> PackageIdentifier -> ModSummary -> Ghc ModuleDescr
parseModuleDescr dflags pid ms = do
    pm <- parseModule ms
    -- The session's CWD is the package dir, so GHC's paths are relative; make
    -- them absolute so the editor can open them from anywhere.
    absSrc <- liftIO $ traverse makeAbsolute (ml_hs_file (ms_location ms))
    let pmKey = PM pid (fromMaybe (error "parseModuleDescr: bad module name")
                          (simpleParse (moduleNameString (moduleName (ms_mod ms)))))
    return ModuleDescr
        { mdModuleId       = pmKey
        , mdMbSourcePath   = absSrc
        , mdReferences     = Map.empty
        , mdIdDescriptions = concatMap (parsedDeclDescr dflags pmKey absSrc)
                                       (hsmodDecls (unLoc (pm_parsed_source pm))) }

-- Top-level declarations from the parsed source give us names + real spans
-- without needing the module to typecheck.  Types/comments aren't taken from
-- here — they come from the .hi data in the merge (mergeDescr).
parsedDeclDescr :: DynFlags -> PackModule -> Maybe FilePath -> LHsDecl GhcPs -> [Descr]
parsedDeclDescr _dflags pm mbSrc (L loc decl) = case decl of
    SigD _ sig                        -> lsig (L loc sig)
    -- Give type-like declarations the same kind of type hint the .hi collector
    -- uses (ClassDescr/DataDescr/TypeDescr), so the merge pairs them with the
    -- .hi description (taking its methods/constructors) instead of leaving a
    -- duplicate.  The empty lists are filled in from the .hi side by the merge.
    TyClD _ ClassDecl{ tcdLName = ln, tcdSigs = sigs }
                                      -> mkDH (occ ln) (locA loc) (ClassDescr [] []) : concatMap lsig sigs
    TyClD _ DataDecl{ tcdLName = ln } -> [ mkDH (occ ln) (locA loc) (DataDescr [] []) ]
    TyClD _ SynDecl{ tcdLName = ln }  -> [ mkDH (occ ln) (locA loc) TypeDescr ]
    TyClD _ FamDecl{ tcdFam = FamilyDecl{ fdLName = ln } } -> [ mkDH (occ ln) (locA loc) TypeDescr ]
    -- Class instances share the class name; pair them up in the merge by the
    -- types they bind (InstanceDescr binds), matching the .hi collector so each
    -- gets its own source span.
    InstD _ (ClsInstD _ ClsInstDecl{ cid_poly_ty = ty }) ->
        case instClassAndBinds ty of
            Just (cls, binds) ->
                [ Real RealDescr
                    { dscName'       = T.pack cls
                    , dscMbTypeStr'  = Nothing
                    , dscMbModu'     = Just pm
                    , dscMbLocation' = atFile mbSrc (srcSpanToLocation (locA loc))
                    , dscMbComment'  = Nothing
                    , dscTypeHint'   = InstanceDescr (map T.pack binds)
                    , dscExported'   = True } ]
            Nothing -> []
    _ -> []
  where
    occ ln = occNameString (rdrNameOcc (unLoc ln))
    mkD name sp = mkDH name sp VariableDescr
    mkDH name sp hint = Real RealDescr
        { dscName'       = T.pack name
        , dscMbTypeStr'  = Nothing
        , dscMbModu'     = Just pm
        , dscMbLocation' = atFile mbSrc (srcSpanToLocation sp)
        , dscMbComment'  = Nothing
        , dscTypeHint'   = hint
        , dscExported'   = True }
    -- type signatures (top-level and class methods) carry the binder names
    lsig (L sloc (TypeSig _ names _))      = [ mkD (occ n) (locA sloc) | n <- names ]
    lsig (L sloc (ClassOpSig _ _ names _)) = [ mkD (occ n) (locA sloc) | n <- names ]
    lsig _ = []

-- Use the (absolute) module source path for the declaration's location file.
atFile :: Maybe FilePath -> Maybe Location -> Maybe Location
atFile (Just f) (Just l) = Just l { locationFile = f }
atFile _        ml       = ml

-- The class name and the head type constructors of an instance head, in the
-- same shape the .hi collector records (class name + roughened arg tycons),
-- so the two descriptions pair up in the merge.  Type-variable arguments have
-- no head tycon and are dropped, matching @catMaybes . ifInstTys@.
instClassAndBinds :: LHsSigType GhcPs -> Maybe (String, [String])
instClassAndBinds (L _ (HsSig{ sig_body = body })) =
    case hsTyConName hd of
        Just cls -> Just (cls, mapMaybe hsTyConName args)
        Nothing  -> Nothing
  where (hd, args) = splitHsAppTy (peelForall body)

peelForall :: LHsType GhcPs -> LHsType GhcPs
peelForall (L _ (HsForAllTy _ _ b)) = peelForall b
peelForall (L _ (HsQualTy _ _ b))   = peelForall b
peelForall t                        = t

splitHsAppTy :: LHsType GhcPs -> (LHsType GhcPs, [LHsType GhcPs])
splitHsAppTy = go []
  where
    go acc (L _ (HsParTy _ t))   = go acc t
    go acc (L _ (HsAppTy _ f x)) = go (x:acc) f
    go acc t                     = (t, acc)

hsTyConName :: LHsType GhcPs -> Maybe String
hsTyConName (L _ (HsParTy _ t))   = hsTyConName t
hsTyConName (L _ (HsAppTy _ f _)) = hsTyConName f
hsTyConName (L _ (HsTyVar _ _ (L _ rdr)))
    | isTcOcc (rdrNameOcc rdr)    = Just (occNameString (rdrNameOcc rdr))
hsTyConName (L _ (HsListTy _ _))      = Just "[]"
hsTyConName (L _ (HsTupleTy _ _ ts))  = Just ("(" ++ replicate (max 0 (length ts - 1)) ',' ++ ")")
hsTyConName _ = Nothing
#else
fillSourceSpans _ _ _ mods = return mods
#endif

interfaceToModuleDescr :: DynFlags -> FilePath -> PackageIdentifier -> Interface -> ModuleDescr
interfaceToModuleDescr dflags _dirPath pid interface =
    ModuleDescr {
        mdModuleId          =   PM pid modName
    ,   mdMbSourcePath      =   Just filepath
    ,   mdReferences        =   imports
    ,   mdIdDescriptions    =   descrs}
    where
        -- ifaceOrigFilename points at the hs output file (not chs file)
        -- So if possible we look up one of the things in the module and
        -- get the file it is located in.
        filepath   = fromMaybe fallbackFile $ listToMaybe
            [locationFile loc | Real RealDescr{dscMbLocation' = Just loc,
                dscMbModu' = Just dscMod} <- descrs, dscMod == PM pid modName,
                filenameMatchesModule (locationFile loc)]
#if MIN_VERSION_haddock_api(2,28,0)
        -- ifaceOrigFilename was removed; no portable source path remains, so
        -- rely on the per-declaration locations above (empty fallback).
        fallbackFile = ""
#else
        fallbackFile = ifaceOrigFilename interface
#endif
        modName    = fromMaybe (error "Can't parse module name") ((simpleParse . moduleNameString . moduleName . ifaceMod) interface)
        filenameMatchesModule fn = components modName `isSuffixOf` splitDirectories (dropExtension fn)
        descrs     = extractDescrs dflags (PM pid modName)
#if MIN_VERSION_haddock_api(2,28,0)
                        (Map.empty :: Map Name DeclInfo) (ifaceExportItems interface)
#else
                        (ifaceDeclMap interface) (ifaceExportItems interface)
#endif
                        (ifaceInstances interface) [] --(ifaceLocals interface)
        imports    = Map.empty --TODO

getDoc :: Documentation Name -> Maybe NDoc
getDoc = fmap _doc . documentationDoc

type DeclInfo = [LHsDecl GhcRn]

#if MIN_VERSION_ghc(9,6,0) && !MIN_VERSION_ghc(9,8,0)
-- haddock-api for GHC 9.6 pre-processes instances into HaddockClsInst.
extractDescrs :: Ord a => DynFlags -> PackModule -> Map a DeclInfo -> [ExportItem GhcRn] -> [HaddockClsInst] -> [b] -> [Descr]
#else
extractDescrs :: Ord a => DynFlags -> PackModule -> Map a DeclInfo -> [ExportItem GhcRn] -> [ClsInst] -> [b] -> [Descr]
#endif
extractDescrs dflags pm _ifaceDeclMap ifaceExportItems' ifaceInstances' _ifaceLocals =
        transformToDescrs dflags pm exportedDeclInfo ++
          map (toDescrInst dflags pm) ifaceInstances'
    where
        exportedDeclInfo :: [(LHsDecl GhcRn, Maybe NDoc, [(Name, Maybe NDoc)])]
        exportedDeclInfo                    =  mapMaybe toDeclInfo  ifaceExportItems'
        toDeclInfo :: ExportItem GhcRn -> Maybe (LHsDecl GhcRn, Maybe NDoc, [(Name, Maybe NDoc)])
#if MIN_VERSION_haddock_api(2,28,0)
        toDeclInfo (ExportDecl ExportD{expDDecl=decl, expDMbDoc=mbDoc, expDSubDocs=subDocs})   =
                                        Just(decl,getDoc $ fst mbDoc,map (\ (a,b) -> (a,getDoc $ fst b)) subDocs)
#else
        toDeclInfo ExportDecl{expItemDecl=decl, expItemMbDoc=mbDoc, expItemSubDocs=subDocs}   =
                                        Just(decl,getDoc $ fst mbDoc,map (\ (a,b) -> (a,getDoc $ fst b)) subDocs)
#endif
        toDeclInfo ExportNoDecl{}         = Nothing
        toDeclInfo ExportGroup{}          = Nothing
        toDeclInfo ExportDoc{}            = Nothing
        toDeclInfo ExportModule{}         = Nothing

transformToDescrs :: DynFlags -> PackModule -> [(LHsDecl GhcRn, Maybe NDoc, [(Name, Maybe NDoc)])] -> [Descr]
transformToDescrs dflags pm = concatMap transformToDescr
    where
#if MIN_VERSION_ghc(8,6,0)
    transformToDescr (L loc (SigD _ (TypeSig _ names typ)), mbComment, _subCommentList) = map nameDescr names
#elif MIN_VERSION_ghc(8,0,0)
    transformToDescr (L loc (SigD (TypeSig names typ)), mbComment, _subCommentList) = map nameDescr names
#else
    transformToDescr (L loc (SigD (TypeSig names typ _)), mbComment, _subCommentList) = map nameDescr names
#endif
      where
        nameDescr name = Real RealDescr {
                dscName'        =   T.pack . getOccString $ unLoc name
            ,   dscMbTypeStr'   =   Just . BS.pack $ getOccString (unLoc name) <> " :: " <> showSDocUnqual dflags (ppr typ)
            ,   dscMbModu'      =   Just pm
            ,   dscMbLocation'  =   srcSpanToLocation (locA loc)
            ,   dscMbComment'   =   toComment dflags mbComment []
            ,   dscTypeHint'    =   VariableDescr
            ,   dscExported'    =   True}

#if MIN_VERSION_ghc(8,6,0)
    transformToDescr (L loc (SigD _ (PatSynSig _ names typ)), mbComment, _subCommentList) = (<$> names) $ \name ->
#elif MIN_VERSION_ghc(8,2,0)
    transformToDescr (L loc (SigD (PatSynSig names typ)), mbComment, _subCommentList) = (<$> names) $ \name ->
#elif MIN_VERSION_ghc(8,0,0)
    transformToDescr (L loc (SigD (PatSynSig name typ)), mbComment, _subCommentList) = pure $
#else
    transformToDescr (L loc (SigD (PatSynSig name _ _ _ typ)), mbComment, _subCommentList) = pure $
#endif
            Real RealDescr {
            dscName'        =   T.pack . getOccString $ unLoc name
        ,   dscMbTypeStr'   =   Just . BS.pack $ getOccString (unLoc name) <> " :: " <> showSDocUnqual dflags (ppr typ)
        ,   dscMbModu'      =   Just pm
        ,   dscMbLocation'  =   srcSpanToLocation (locA loc)
        ,   dscMbComment'   =   toComment dflags mbComment []
        ,   dscTypeHint'    =   PatternSynonymDescr
        ,   dscExported'    =   True}

#if MIN_VERSION_ghc(8,0,0)
#if MIN_VERSION_ghc(8,6,0)
    transformToDescr (L loc (SigD _ (ClassOpSig _ _ names typ)), mbComment, _subCommentList) =
#else
    transformToDescr (L loc (SigD (ClassOpSig _ names typ)), mbComment, _subCommentList) =
#endif
        map (\name ->
            Real RealDescr {
            dscName'        =   T.pack . getOccString $ unLoc name
        ,   dscMbTypeStr'   =   Just . BS.pack $ getOccString (unLoc name) <> " :: " <> showSDocUnqual dflags (ppr typ)
        ,   dscMbModu'      =   Just pm
        ,   dscMbLocation'  =   srcSpanToLocation (locA loc)
        ,   dscMbComment'   =   toComment dflags mbComment []
        ,   dscTypeHint'    =   PatternSynonymDescr
        ,   dscExported'    =   True}) names
#endif

    transformToDescr (L _loc SigD{}, _mbComment, _subCommentList) = []

#if MIN_VERSION_ghc(8,6,0)
    transformToDescr (L loc for@(ForD _ (ForeignImport _ lid _ _)), mbComment, _sigList) =
#else
    transformToDescr (L loc for@(ForD (ForeignImport lid _ _ _)), mbComment, _sigList) =
#endif
        [Real RealDescr {
        dscName'        =   T.pack . getOccString $ unLoc lid
    ,   dscMbTypeStr'   =   Just . BS.pack . showSDocUnqual dflags $ ppr for
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation (locA loc)
    ,   dscMbComment'   =   toComment dflags mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]

#if MIN_VERSION_ghc(8,6,0)
    transformToDescr (L loc (TyClD _ typ@FamDecl{tcdFam = FamilyDecl{fdLName = lid}}), mbComment,_sigList) =
#else
    transformToDescr (L loc (TyClD typ@FamDecl{tcdFam = FamilyDecl{fdLName = lid}}), mbComment,_sigList) =
#endif
        [Real RealDescr {
        dscName'        =   T.pack . getOccString $ unLoc lid
    ,   dscMbTypeStr'   =   Just . BS.pack . showSDocUnqual dflags $ ppr typ
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation (locA loc)
    ,   dscMbComment'   =   toComment dflags mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]

#if MIN_VERSION_ghc(8,6,0)
    transformToDescr (L loc (TyClD _ typ@SynDecl{tcdLName = lid}), mbComment,_sigList) =
#else
    transformToDescr (L loc (TyClD typ@SynDecl{tcdLName = lid}), mbComment,_sigList) =
#endif
        [Real RealDescr {
        dscName'        =   T.pack . getOccString $ unLoc lid
    ,   dscMbTypeStr'   =   Just . BS.pack . showSDocUnqual dflags $ ppr typ
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation (locA loc)
    ,   dscMbComment'   =   toComment dflags mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]

#if MIN_VERSION_ghc(8,6,0)
    transformToDescr (L loc (TyClD _ typ@DataDecl{tcdLName = lid,
#else
    transformToDescr (L loc (TyClD typ@DataDecl{tcdLName = lid,
#endif
                                                tcdDataDefn =
                                                  HsDataDefn{dd_cons = lConDecl, dd_derivs = tcdDerivs'}}), mbComment,_sigList) =
        Real RealDescr {
        dscName'        =   T.pack name
    ,   dscMbTypeStr'   =   Just . BS.pack . showSDocUnqual dflags . ppr $ uncommentData typ
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation (locA loc)
    ,   dscMbComment'   =   toComment dflags mbComment []
    ,   dscTypeHint'    =   DataDescr constructors fields
    ,   dscExported'    =   True}
            : derivings tcdDerivs'
        where
        constructors    =   concatMap (extractConstructor dflags) lConDecl
        fields          =   nub $ concatMap (extractRecordFields dflags) lConDecl
        name            =   getOccString (unLoc lid)
        derivings :: HsDeriving GhcRn -> [Descr]
        derivings _l = [] -- concatMap (extractDeriving dflags pm name) (unLoc l)

#if MIN_VERSION_ghc(8,6,0)
    transformToDescr (L loc (TyClD _ cl@ClassDecl{tcdLName = tcdLName', tcdSigs = tcdSigs', tcdDocs = docs}), mbComment,_subCommentList) =
#else
    transformToDescr (L loc (TyClD cl@ClassDecl{tcdLName = tcdLName', tcdSigs = tcdSigs', tcdDocs = docs}), mbComment,_subCommentList) =
#endif
        [Real RealDescr {
        dscName'        =   T.pack . getOccString $ unLoc tcdLName'
    ,   dscMbTypeStr'   =   Just . BS.pack . showSDocUnqual dflags $ ppr cl{tcdMeths = emptyLHsBinds}
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation (locA loc)
    ,   dscMbComment'   =   toComment dflags mbComment []
    ,   dscTypeHint'    =   ClassDescr super methods
    ,   dscExported'    =   True    }]
        where
        methods         =   extractMethods dflags tcdSigs' docs
        super           =   []

    transformToDescr (_, _mbComment, _sigList) = []

#if MIN_VERSION_ghc(9,6,0) && !MIN_VERSION_ghc(9,8,0)
-- haddock-api for GHC 9.6 hands us a pre-digested HaddockClsInst.
toDescrInst :: DynFlags -> PackModule -> HaddockClsInst -> Descr
toDescrInst _dflags pm HaddockClsInst{ haddockClsInstClsName = clsName
                                     , haddockClsInstName    = instName
                                     , haddockClsInstPprHoogle = mbHoogle } =
        Real RealDescr {
        dscName'        =   T.pack $ getOccString clsName
    ,   dscMbTypeStr'   =   Just . BS.pack $ fromMaybe (getOccString clsName) mbHoogle
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation (getSrcSpan instName)
    ,   dscMbComment'   =   Nothing
    ,   dscTypeHint'    =   InstanceDescr []
    ,   dscExported'    =   True}
#else
toDescrInst :: DynFlags -> PackModule -> ClsInst -> Descr
toDescrInst dflags pm inst@ClsInst{is_cls = is_cls', is_tys = is_tys'} =
        Real RealDescr {
        dscName'        =   T.pack $ getOccString is_cls'
    ,   dscMbTypeStr'   =   Just . BS.pack . showSDocUnqual dflags $ ppr inst
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation (getSrcSpan inst)
    ,   dscMbComment'   =   Nothing
    ,   dscTypeHint'    =   InstanceDescr (map (T.pack . showSDocUnqual dflags . ppr) is_tys')
    ,   dscExported'    =   True}
#endif

extractMethods :: DynFlags -> [LSig GhcRn] -> [MyLDocDecl] -> [SimpleDescr]
extractMethods dflags sigs docs =
    let pairs = attachComments' dflags sigs docs
    in concatMap (extractMethod dflags) pairs

extractMethod :: DynFlags -> (LHsDecl GhcRn, Maybe NDoc) -> [SimpleDescr]
#if MIN_VERSION_ghc(8,6,0)
extractMethod dflags (L loc (SigD _ ts@(TypeSig _ names _typ)), mbDoc) = map (extractMethodName dflags (locA loc) ts mbDoc) names
extractMethod dflags (L loc (SigD _ ts@(PatSynSig _ names _typ)), mbDoc) = map (extractMethodName dflags (locA loc) ts mbDoc) names
extractMethod dflags (L loc (SigD _ ts@(ClassOpSig _ _ names _typ)), mbDoc) = map (extractMethodName dflags (locA loc) ts mbDoc) names
#else
#if MIN_VERSION_ghc(8,0,0)
extractMethod dflags (L loc (SigD ts@(TypeSig names _typ)), mbDoc) = map (extractMethodName dflags (locA loc) ts mbDoc) names
#if MIN_VERSION_ghc(8,2,0)
extractMethod dflags (L loc (SigD ts@(PatSynSig names _typ)), mbDoc) = map (extractMethodName dflags (locA loc) ts mbDoc) names
#else
extractMethod dflags (L loc (SigD ts@(PatSynSig name _typ)), mbDoc) = [extractMethodName dflags (locA loc) ts mbDoc name]
#endif
extractMethod dflags (L loc (SigD ts@(ClassOpSig _ names _typ)), mbDoc) = map (extractMethodName dflags (locA loc) ts mbDoc) names
#else
extractMethod dflags (L loc (SigD ts@(TypeSig names _typ _)), mbDoc) = map (extractMethodName dflags (locA loc) ts mbDoc) names
#endif
#endif
extractMethod _ _ = []

extractMethodName :: (NamedThing a, Outputable o
#if MIN_VERSION_ghc(8,8,0) && !MIN_VERSION_ghc(9,0,0)
  , HasSrcSpan (GenLocated l a)
#endif
  ) => DynFlags
                           -> SrcSpan -> o -> Maybe NDoc -> GenLocated l a -> SimpleDescr
extractMethodName dflags loc ts mbDoc name =
    SimpleDescr
        (T.pack . getOccString $ unLoc name)
        (Just . BS.pack . showSDocUnqual dflags $ ppr ts)
        (srcSpanToLocation loc)
        (toComment dflags mbDoc [])
        True

extractConstructor :: DynFlags -> LConDecl GhcRn -> [SimpleDescr]
#if MIN_VERSION_ghc(8,0,0)
extractConstructor dflags decl@(L loc d) = extractDecl d
 where
  extractDecl (ConDeclGADT {..}) = map (extractName con_doc) (toList con_names)
  extractDecl (ConDeclH98 {..}) = [extractName con_doc con_name]
#if MIN_VERSION_ghc(8,6,0)
  extractDecl (XConDecl {}) = []
#endif
#else
extractConstructor dflags decl@(L loc ConDecl{con_names = names, con_doc = doc}) =
  map (extractName doc) names
  where
#endif
  extractName doc name =
    SimpleDescr
        (T.pack . getOccString $ unLoc name)
        (Just . BS.pack . showSDocUnqual dflags . ppr $uncommentDecl decl)
        (srcSpanToLocation (locA loc))
        (case doc of
            Nothing -> Nothing
#if MIN_VERSION_ghc(9,0,0)
            Just ld -> Just . BS.pack . T.unpack $ printHsDoc (hsDocString (unLoc ld)))
#else
            Just (L _ d') -> Just . BS.pack . T.unpack $ printHsDoc d')
#endif
        True

extractRecordFields :: DynFlags -> LConDecl GhcRn -> [SimpleDescr]
#if MIN_VERSION_ghc(8,6,0)
extractRecordFields dflags (L _ _decl@ConDeclH98 {con_args = RecCon flds}) =
#elif MIN_VERSION_ghc(8,0,0)
extractRecordFields dflags (L _ _decl@ConDeclH98 {con_details = RecCon flds}) =
#else
extractRecordFields dflags (L _ _decl@ConDecl{con_details = (RecCon flds)}) =
#endif
    concatMap extractRecordFields' (unLoc flds)
    where
#if MIN_VERSION_ghc(9,14,0)
    extractRecordFields' (L _ (HsConDeclRecField _ names spec)) = map extractName names
#elif MIN_VERSION_ghc(8,6,0)
    extractRecordFields' :: LConDeclField GhcRn -> [SimpleDescr]
    extractRecordFields' (L _ (XConDeclField _)) = []
    extractRecordFields' (L _ _field@(ConDeclField _ names typ doc)) = map extractName names
#else
    extractRecordFields' :: LConDeclField GhcRn -> [SimpleDescr]
    extractRecordFields' (L _ _field@(ConDeclField names typ doc)) = map extractName names
#endif
      where
#if MIN_VERSION_ghc(9,14,0)
      typ = cdf_type spec
#endif
      extractName name =
        SimpleDescr
            (T.pack . showSDoc dflags . ppr $ unLoc name)
            (Just (BS.pack (showSDocUnqual dflags $ ppr typ)))
#if MIN_VERSION_ghc(9,0,0)
            (srcSpanToLocation $ getLocA name)
            Nothing
#else
            (srcSpanToLocation $ getLoc name)
            (case doc of
                Nothing -> Nothing
                Just (L _ d) -> Just . BS.pack . T.unpack $ printHsDoc d)
#endif
            True
#if MIN_VERSION_ghc(8,0,0)
extractRecordFields dflags (L _ _decl@ConDeclGADT
#if MIN_VERSION_ghc(8,6,0)
        {con_names = names, con_res_ty = typ, con_doc = doc}) =
#else
        {con_names = names, con_type = typ, con_doc = doc}) =
#endif
    map extractName (toList names)
  where
    extractName name =
        SimpleDescr
            (T.pack . showSDoc dflags . ppr $ unLoc name)
            (Just (BS.pack (showSDocUnqual dflags $ ppr typ)))
#if MIN_VERSION_ghc(9,0,0)
            (srcSpanToLocation $ getLocA name)
            Nothing
#else
            (srcSpanToLocation $ getLoc name)
            (case doc of
                Nothing -> Nothing
                Just (L _ d) -> Just . BS.pack . T.unpack $ printHsDoc d)
#endif
            True
#endif
extractRecordFields _ _ = []

toComment :: DynFlags -> Maybe NDoc -> [NDoc] -> Maybe ByteString
toComment dflags (Just c) _    =  Just . BS.pack . T.unpack $ printHsDoc' dflags c
toComment dflags Nothing (c:_) =  Just . BS.pack . T.unpack $ printHsDoc' dflags c
toComment _ Nothing []         =  Nothing


{--
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [Data] []
collectParseInfoForDecl (l,st) ((Just (L loc (TyClD (TyFamily _ lid _ _)))), mbComment')
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [] []
collectParseInfoForDecl (l,st) ((Just (L loc (TyClD (ClassDecl _ lid _ _ _ _ _ _ )))), mbComment')
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [Class] []
--}

printHsDoc' :: DynFlags -> NDoc  -> Text
printHsDoc' dflags d = T.pack . show $ PPDoc dflags d

data PPDoc alpha = PPDoc DynFlags (HsDoc alpha)

instance Outputable alpha => Show (PPDoc alpha)  where
    showsPrec _ (PPDoc _ DocEmpty)                 =   id
    showsPrec _ (PPDoc d (DocAppend l r))          =   shows (PPDoc d l)  . shows (PPDoc d r)
    showsPrec _ (PPDoc _ (DocString str))          =   showString str
    showsPrec _ (PPDoc d (DocParagraph doc))       =   shows (PPDoc d doc) . showChar '\n'
    showsPrec _ (PPDoc d (DocIdentifier l))        =   foldr (\i _f -> showChar '\'' .
                                                     (showString . showSDoc d . ppr) i . showChar '\'') id [l]
#if MIN_VERSION_haddock_library(1,10,0)
    showsPrec _ (PPDoc _ (DocModule (ModLink str _))) = showChar '"' . showString str . showChar '"'
#else
    showsPrec _ (PPDoc _ (DocModule str))          =   showChar '"' . showString str . showChar '"'
#endif
    showsPrec _ (PPDoc d (DocEmphasis doc))        =   showChar '/' . shows (PPDoc d doc)  . showChar '/'
    showsPrec _ (PPDoc d (DocMonospaced doc))      =   showChar '@' . shows (PPDoc d doc) . showChar '@'
    showsPrec _ (PPDoc d (DocUnorderedList l))     =
        foldr (\s r -> showString "* " . shows (PPDoc d s) . showChar '\n' . r) id l
    showsPrec _ (PPDoc d (DocOrderedList l))       =
#if MIN_VERSION_haddock_library(1,11,0)
        -- list items carry their number since haddock-library 1.11
        foldr (\(n,i) _f -> shows n . showSpace .  shows (PPDoc d i)) id l
#else
        foldr (\(i,n) _f -> shows n . showSpace .  shows (PPDoc d i)) id (zip l [1 .. length l])
#endif
    showsPrec _ (PPDoc d (DocDefList li))          =
        foldr (\(l,r) f -> showString "[@" . shows (PPDoc d l) . showString "[@ " . shows (PPDoc d r) . f) id li
    showsPrec _ (PPDoc d (DocCodeBlock doc))      =   showChar '@' . shows (PPDoc d doc) . showChar '@'
    showsPrec _ (PPDoc d (DocHyperlink (Hyperlink url lbl)))  =   showChar '<' . showString url . showChar '>'
    showsPrec _ (PPDoc _ (DocAName str))          =   showChar '#' . showString str . showChar '#'
    showsPrec _ (PPDoc _ _)                       =   id

attachComments' :: DynFlags -> [LSig GhcRn] -> [MyLDocDecl] -> [(LHsDecl GhcRn, Maybe NDoc)]
attachComments' dflags sigs docs = collectDocs' dflags $ sortByLoc
#if MIN_VERSION_ghc(8,10,2)
    (map (\ (L l i) -> L l (SigD NoExtField i)) sigs ++
        map (\ (L l i) -> L l (DocD NoExtField i)) docs)
#elif MIN_VERSION_ghc(8,6,0)
    (map (\ (L l i) -> L l (SigD NoExt i)) sigs ++
        map (\ (L l i) -> L l (DocD NoExt i)) docs)
#else
    (map (\ (L l i) -> L l (SigD i)) sigs ++
        map (\ (L l i) -> L l (DocD i)) docs)
#endif

-- | Collect the docs and attach them to the right declaration.
collectDocs' :: DynFlags -> [LHsDecl GhcRn] -> [(LHsDecl GhcRn, Maybe NDoc)]
collectDocs' dflags = collect' dflags Nothing DocEmpty

collect' :: DynFlags -> Maybe (LHsDecl GhcRn) -> NDoc -> [LHsDecl GhcRn] -> [(LHsDecl GhcRn, Maybe NDoc)]
collect' _dflags d doc_so_far [] =
   case d of
        Nothing -> []
        Just d0  -> finishedDoc' d0 doc_so_far []

collect' dflags d doc_so_far (e:es) =
  case e of
#if MIN_VERSION_ghc(8,6,0)
    L _ (DocD _ (DocCommentNext str)) ->
#else
    L _ (DocD (DocCommentNext str)) ->
#endif
      case d of
        Nothing -> collect' dflags d (DocAppend doc_so_far (DocString (show' dflags str))) es
        Just d0 -> finishedDoc' d0 doc_so_far (collect' dflags Nothing (DocString (show' dflags str)) es)

#if MIN_VERSION_ghc(8,6,0)
    L _ (DocD _ (DocCommentPrev str)) -> collect' dflags d (DocAppend doc_so_far (DocString (show' dflags str))) es
#else
    L _ (DocD (DocCommentPrev str)) -> collect' dflags d (DocAppend doc_so_far (DocString (show' dflags str))) es
#endif

    _ -> case d of
      Nothing -> collect' dflags (Just e) doc_so_far es
      Just d0 -> finishedDoc' d0 doc_so_far (collect' dflags (Just e) DocEmpty es)

finishedDoc' :: LHsDecl GhcRn -> NDoc -> [(LHsDecl GhcRn, Maybe NDoc)]
                    -> [(LHsDecl GhcRn, Maybe NDoc)]
finishedDoc' d doc rest | isEmptyDoc doc = (d, Nothing) : rest
finishedDoc' d doc rest | notDocDecl d   = (d, Just doc) : rest
  where
#if MIN_VERSION_ghc(8,6,0)
    notDocDecl (L _ (DocD _ _)) = False
#else
    notDocDecl (L _ (DocD _)) = False
#endif
    notDocDecl _              = True
finishedDoc' _ _ rest = rest
