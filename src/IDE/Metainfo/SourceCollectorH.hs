{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
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

import Documentation.Haddock
import Distribution.Text (simpleParse, display)
import InstEnv (ClsInst(..))
import Data.Map (Map)
import qualified Data.Map as Map (empty)

import Data.List (nub, isSuffixOf)
import qualified Data.ByteString.Char8 as BS (pack)
import IDE.Metainfo.WorkspaceCollector
       (srcSpanToLocation, uncommentDecl, uncommentData, printHsDoc, sortByLoc)
import PackageConfig (PackageConfig)
import Distribution.Verbosity (verbose, normal)
#if MIN_VERSION_ghc(8,2,0)
import GHC.PackageDb (exposedModules, hiddenModules)
import Module (ModuleName, Module)
#else
import GHC.PackageDb (exposedModules, hiddenModules, exposedName)
#endif
import Documentation.Haddock.Types (_doc)
import IDE.StrippedPrefs (getUnpackDirectory, Prefs(..))
import IDE.Metainfo.SourceDB (sourceForPackage, getSourcesMap)
import MonadUtils (liftIO)
import System.Directory (setCurrentDirectory, doesDirectoryExist, createDirectoryIfMissing, doesFileExist)
import System.FilePath ((<.>), dropFileName, (</>), splitDirectories, dropExtension, takeDirectory)
import System.Exit (ExitCode(..))
import Data.Maybe(mapMaybe, listToMaybe, fromMaybe)
import IDE.Utils.GHCUtils (inGhcIO)
import qualified Control.Exception as NewException (SomeException, catch)
import IDE.Utils.Tool
import IDE.Utils.FileUtils (figureOutGhcOpts', myCanonicalizePath, getSysLibDir)
import Distribution.Package(PackageIdentifier, pkgName)
import Distribution.Simple.Utils (installDirectoryContents)
import GHC hiding(Id,Failed,Succeeded,ModuleName)
import Distribution.ModuleName (components)
import System.Log.Logger (warningM, debugM)
import Control.DeepSeq (deepseq)
import Data.ByteString.Char8 (ByteString)
import Outputable hiding(trace, (<>))
import GHC.Show(showSpace)
import Name

#if MIN_VERSION_ghc(8,2,0)
exposedName :: (ModuleName, Maybe Module) -> ModuleName
exposedName = fst
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

type MyLDocDecl = LDocDecl

show' :: Outputable alpha => DynFlags -> alpha  -> String
show' dflags = showSDoc dflags . ppr

data PackageCollectStats = PackageCollectStats {
    packageString       :: Text,
    modulesTotal        :: Maybe Int,
    withSource          :: Bool,
    retrieved           :: Bool,
    mbError             :: Maybe Text}

findSourceForPackage :: Prefs -> PackageIdentifier -> Maybe FilePath -> IO (Either Text FilePath)
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


copyNixSource :: PackageIdentifier -> FilePath -> FilePath -> IO Bool
copyNixSource packageId fpUnpack project = return False
copyNixSource packageId fpUnpack project = do
    debugM "leksah-server" "copyNixSource"
    let nixFile = takeDirectory project </> "default.nix"
    doesFileExist nixFile >>= \case
        True -> do
            (nixOuput, _) <- runTool' "nix-instantiate" [T.pack nixFile, "--eval", "-A", T.pack $ "ghc." <> display (pkgName packageId) <> ".src"] (Just $ takeDirectory project) Nothing
            case reverse nixOuput of
                (ToolExit ExitSuccess:ToolOutput lineMaybeQuoted:_) -> do
                    let line = removeQuotes lineMaybeQuoted
                        cabalFile = T.unpack line </> display (pkgName packageId) <.> "cabal"
                    doesFileExist cabalFile >>= \case
                        True -> installDirectoryContents normal (T.unpack line) (fpUnpack </> packageName') >> return True
                        False -> do
                            debugM "leksah-server" $ "copyNixSource cabal file not found " <> cabalFile
                            return False
                _ -> return False
        False -> return False
    where
        packageName = packageIdentifierToString packageId
        packageName' = T.unpack packageName
        removeQuotes s = fromMaybe s $ T.stripPrefix "\"" s >>= T.stripSuffix "\""

packageFromSource :: Maybe FilePath -> [FilePath] -> FilePath -> PackageConfig -> IO (Maybe PackageDescr, PackageCollectStats)
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
        debugM "leksah-server" $ "Could not find system lib dir for GHC " <> VERSION_ghc <> " (used to build Leksah)"
        return (Nothing, PackageCollectStats packageName Nothing False False
                  (Just ("Ghc failed to process: could not find " <> VERSION_ghc <> " system lib dir (" <> T.pack cabalPath <> ")")))
      Just libDir -> inGhcIO libDir ghcFlags [Opt_Haddock] dbs $ \ dflags -> do
        (interfaces,_) <- processModules verbose (exportedMods ++ hiddenMods) [] []
        liftIO $ print (length interfaces)
        let mods = map (interfaceToModuleDescr dflags dirPath (packId $ getThisPackage packageConfig)) interfaces
        sp <- liftIO $ myCanonicalizePath dirPath
        let pd = PackageDescr {
                pdPackage           =   packId (getThisPackage packageConfig)
            ,   pdModules           =   mods
            ,   pdBuildDepends      =   [] -- TODO depends packageConfig
            ,   pdMbSourcePath      =   Just sp}
        let stat = PackageCollectStats packageName (Just (length mods)) True False Nothing
        liftIO $ deepseq pd $ return (Just pd, stat)
  where
    exportedMods = map (moduleNameString . exposedName) $ exposedModules packageConfig
    hiddenMods   = map moduleNameString $ hiddenModules packageConfig
    dirPath      = dropFileName cabalPath
    packageName  = packageIdentifierToString (packId $ getThisPackage packageConfig)

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
        filepath   = fromMaybe (ifaceOrigFilename interface) $ listToMaybe
            [locationFile loc | Real RealDescr{dscMbLocation' = Just loc,
                dscMbModu' = Just dscMod} <- descrs, dscMod == PM pid modName,
                filenameMatchesModule (locationFile loc)]
        modName    = fromMaybe (error "Can't parse module name") ((simpleParse . moduleNameString . moduleName . ifaceMod) interface)
        filenameMatchesModule fn = components modName `isSuffixOf` splitDirectories (dropExtension fn)
        descrs     = extractDescrs dflags (PM pid modName)
                        (ifaceDeclMap interface) (ifaceExportItems interface)
                        (ifaceInstances interface) [] --(ifaceLocals interface)
        imports    = Map.empty --TODO

getDoc :: Documentation Name -> Maybe NDoc
getDoc = fmap _doc . documentationDoc

type DeclInfo = [LHsDecl GhcRn]

extractDescrs :: Ord a => DynFlags -> PackModule -> Map a DeclInfo -> [ExportItem GhcRn] -> [ClsInst] -> [b] -> [Descr]
extractDescrs dflags pm _ifaceDeclMap ifaceExportItems' ifaceInstances' _ifaceLocals =
        transformToDescrs dflags pm exportedDeclInfo ++
          map (toDescrInst dflags pm) ifaceInstances'
    where
        exportedDeclInfo :: [(LHsDecl GhcRn, Maybe NDoc, [(Name, Maybe NDoc)])]
        exportedDeclInfo                    =  mapMaybe toDeclInfo  ifaceExportItems'
        toDeclInfo :: ExportItem GhcRn -> Maybe (LHsDecl GhcRn, Maybe NDoc, [(Name, Maybe NDoc)])
        toDeclInfo ExportDecl{expItemDecl=decl, expItemMbDoc=mbDoc, expItemSubDocs=subDocs}   =
                                        Just(decl,getDoc $ fst mbDoc,map (\ (a,b) -> (a,getDoc $ fst b)) subDocs)
        toDeclInfo ExportNoDecl{}         = Nothing
        toDeclInfo ExportGroup{}          = Nothing
        toDeclInfo ExportDoc{}            = Nothing
        toDeclInfo ExportModule{}         = Nothing

transformToDescrs :: DynFlags -> PackModule -> [(LHsDecl GhcRn, Maybe NDoc, [(Name, Maybe NDoc)])] -> [Descr]
transformToDescrs dflags pm = concatMap transformToDescr
    where
#if MIN_VERSION_ghc(8,0,0)
    transformToDescr (L loc (SigD (TypeSig names typ)), mbComment, _subCommentList) = map nameDescr names
#else
    transformToDescr (L loc (SigD (TypeSig names typ _)), mbComment, _subCommentList) = map nameDescr names
#endif
      where
        nameDescr name = Real RealDescr {
                dscName'        =   T.pack . getOccString $ unLoc name
            ,   dscMbTypeStr'   =   Just . BS.pack $ getOccString (unLoc name) <> " :: " <> showSDocUnqual dflags (ppr typ)
            ,   dscMbModu'      =   Just pm
            ,   dscMbLocation'  =   srcSpanToLocation loc
            ,   dscMbComment'   =   toComment dflags mbComment []
            ,   dscTypeHint'    =   VariableDescr
            ,   dscExported'    =   True}

#if MIN_VERSION_ghc(8,2,0)
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
        ,   dscMbLocation'  =   srcSpanToLocation loc
        ,   dscMbComment'   =   toComment dflags mbComment []
        ,   dscTypeHint'    =   PatternSynonymDescr
        ,   dscExported'    =   True}

#if MIN_VERSION_ghc(8,0,0)
    transformToDescr (L loc (SigD (ClassOpSig _ names typ)), mbComment, _subCommentList) =
        map (\name ->
            Real RealDescr {
            dscName'        =   T.pack . getOccString $ unLoc name
        ,   dscMbTypeStr'   =   Just . BS.pack $ getOccString (unLoc name) <> " :: " <> showSDocUnqual dflags (ppr typ)
        ,   dscMbModu'      =   Just pm
        ,   dscMbLocation'  =   srcSpanToLocation loc
        ,   dscMbComment'   =   toComment dflags mbComment []
        ,   dscTypeHint'    =   PatternSynonymDescr
        ,   dscExported'    =   True}) names
#endif

    transformToDescr (L _loc (SigD _), _mbComment, _subCommentList) = []

    transformToDescr (L loc for@(ForD (ForeignImport lid _ _ _)), mbComment, _sigList) =
        [Real RealDescr {
        dscName'        =   T.pack . getOccString $ unLoc lid
    ,   dscMbTypeStr'   =   Just . BS.pack . showSDocUnqual dflags $ ppr for
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment dflags mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]

    transformToDescr (L loc (TyClD typ@FamDecl{tcdFam = FamilyDecl{fdLName = lid}}), mbComment,_sigList) =
        [Real RealDescr {
        dscName'        =   T.pack . getOccString $ unLoc lid
    ,   dscMbTypeStr'   =   Just . BS.pack . showSDocUnqual dflags $ ppr typ
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment dflags mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]

    transformToDescr (L loc (TyClD typ@SynDecl{tcdLName = lid}), mbComment,_sigList) =
        [Real RealDescr {
        dscName'        =   T.pack . getOccString $ unLoc lid
    ,   dscMbTypeStr'   =   Just . BS.pack . showSDocUnqual dflags $ ppr typ
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment dflags mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]

    transformToDescr (L loc (TyClD typ@DataDecl{tcdLName = lid,
                                                tcdDataDefn =
                                                  HsDataDefn{dd_cons = lConDecl, dd_derivs = tcdDerivs'}}), mbComment,_sigList) =
        Real RealDescr {
        dscName'        =   T.pack name
    ,   dscMbTypeStr'   =   Just . BS.pack . showSDocUnqual dflags . ppr $ uncommentData typ
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
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

    transformToDescr (L loc (TyClD cl@ClassDecl{tcdLName = tcdLName', tcdSigs = tcdSigs', tcdDocs = docs}), mbComment,_subCommentList) =
        [Real RealDescr {
        dscName'        =   T.pack . getOccString $ unLoc tcdLName'
    ,   dscMbTypeStr'   =   Just . BS.pack . showSDocUnqual dflags $ ppr cl{tcdMeths = emptyLHsBinds}
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment dflags mbComment []
    ,   dscTypeHint'    =   ClassDescr super methods
    ,   dscExported'    =   True    }]
        where
        methods         =   extractMethods dflags tcdSigs' docs
        super           =   []

    transformToDescr (_, _mbComment, _sigList) = []

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

extractMethods :: DynFlags -> [LSig GhcRn] -> [MyLDocDecl] -> [SimpleDescr]
extractMethods dflags sigs docs =
    let pairs = attachComments' dflags sigs docs
    in concatMap (extractMethod dflags) pairs

extractMethod :: DynFlags -> (LHsDecl GhcRn, Maybe NDoc) -> [SimpleDescr]
#if MIN_VERSION_ghc(8,0,0)
extractMethod dflags (L loc (SigD ts@(TypeSig names _typ)), mbDoc) = map (extractMethodName dflags loc ts mbDoc) names
#if MIN_VERSION_ghc(8,2,0)
extractMethod dflags (L loc (SigD ts@(PatSynSig names _typ)), mbDoc) = map (extractMethodName dflags loc ts mbDoc) names
#else
extractMethod dflags (L loc (SigD ts@(PatSynSig name _typ)), mbDoc) = [extractMethodName dflags loc ts mbDoc name]
#endif
extractMethod dflags (L loc (SigD ts@(ClassOpSig _ names _typ)), mbDoc) = map (extractMethodName dflags loc ts mbDoc) names
#else
extractMethod dflags (L loc (SigD ts@(TypeSig names _typ _)), mbDoc) = map (extractMethodName dflags loc ts mbDoc) names
#endif
extractMethod _ _ = []

extractMethodName :: (NamedThing a, Outputable o) => DynFlags
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
  extractDecl (ConDeclGADT {..}) = map (extractName con_doc) con_names
  extractDecl (ConDeclH98 {..}) = [extractName con_doc con_name]
#else
extractConstructor dflags decl@(L loc ConDecl{con_names = names, con_doc = doc}) =
  map (extractName doc) names
  where
#endif
  extractName doc name =
    SimpleDescr
        (T.pack . getOccString $ unLoc name)
        (Just . BS.pack . showSDocUnqual dflags . ppr $uncommentDecl decl)
        (srcSpanToLocation loc)
        (case doc of
            Nothing -> Nothing
            Just (L _ d') -> Just . BS.pack . T.unpack $ printHsDoc d')
        True

extractRecordFields :: DynFlags -> LConDecl GhcRn -> [SimpleDescr]
#if MIN_VERSION_ghc(8,0,0)
extractRecordFields dflags (L _ _decl@ConDeclH98 {con_details = RecCon flds}) =
#else
extractRecordFields dflags (L _ _decl@ConDecl{con_details = (RecCon flds)}) =
#endif
    concatMap extractRecordFields' (unLoc flds)
    where
    extractRecordFields' :: LConDeclField GhcRn -> [SimpleDescr]
    extractRecordFields' (L _ _field@(ConDeclField names typ doc)) = map extractName names
      where
      extractName name =
        SimpleDescr
            (T.pack . showSDoc dflags . ppr $ unLoc name)
            (Just (BS.pack (showSDocUnqual dflags $ ppr typ)))
            (srcSpanToLocation $ getLoc name)
            (case doc of
                Nothing -> Nothing
                Just (L _ d) -> Just . BS.pack . T.unpack $ printHsDoc d)
            True
#if MIN_VERSION_ghc(8,0,0)
extractRecordFields dflags (L _ _decl@ConDeclGADT
        {con_names = names, con_type = typ, con_doc = doc}) =
    map extractName names
  where
    extractName name =
        SimpleDescr
            (T.pack . showSDoc dflags . ppr $ unLoc name)
            (Just (BS.pack (showSDocUnqual dflags $ ppr typ)))
            (srcSpanToLocation $ getLoc name)
            (case doc of
                Nothing -> Nothing
                Just (L _ d) -> Just . BS.pack . T.unpack $ printHsDoc d)
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
    showsPrec _ (PPDoc _ (DocModule str))          =   showChar '"' . showString str . showChar '"'
    showsPrec _ (PPDoc d (DocEmphasis doc))        =   showChar '/' . shows (PPDoc d doc)  . showChar '/'
    showsPrec _ (PPDoc d (DocMonospaced doc))      =   showChar '@' . shows (PPDoc d doc) . showChar '@'
    showsPrec _ (PPDoc d (DocUnorderedList l))     =
        foldr (\s r -> showString "* " . shows (PPDoc d s) . showChar '\n' . r) id l
    showsPrec _ (PPDoc d (DocOrderedList l))       =
        foldr (\(i,n) _f -> shows n . showSpace .  shows (PPDoc d i)) id (zip l [1 .. length l])
    showsPrec _ (PPDoc d (DocDefList li))          =
        foldr (\(l,r) f -> showString "[@" . shows (PPDoc d l) . showString "[@ " . shows (PPDoc d r) . f) id li
    showsPrec _ (PPDoc d (DocCodeBlock doc))      =   showChar '@' . shows (PPDoc d doc) . showChar '@'
    showsPrec _ (PPDoc _ (DocHyperlink h))            =   showChar '<' . showString (show h) . showChar '>'
    showsPrec _ (PPDoc _ (DocAName str))          =   showChar '#' . showString str . showChar '#'
    showsPrec _ (PPDoc _ _)                       =   id

attachComments' :: DynFlags -> [LSig GhcRn] -> [MyLDocDecl] -> [(LHsDecl GhcRn, Maybe NDoc)]
attachComments' dflags sigs docs = collectDocs' dflags $ sortByLoc
                                                           (map (\ (L l i) -> L l (SigD i)) sigs ++
                                                              map (\ (L l i) -> L l (DocD i)) docs)

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
    L _ (DocD (DocCommentNext str)) ->
      case d of
        Nothing -> collect' dflags d (DocAppend doc_so_far (DocString (show' dflags str))) es
        Just d0 -> finishedDoc' d0 doc_so_far (collect' dflags Nothing (DocString (show' dflags str)) es)

    L _ (DocD (DocCommentPrev str)) -> collect' dflags d (DocAppend doc_so_far (DocString (show' dflags str))) es

    _ -> case d of
      Nothing -> collect' dflags (Just e) doc_so_far es
      Just d0 -> finishedDoc' d0 doc_so_far (collect' dflags (Just e) DocEmpty es)

finishedDoc' :: LHsDecl alpha -> NDoc -> [(LHsDecl alpha, Maybe NDoc)]
                    -> [(LHsDecl alpha, Maybe NDoc)]
finishedDoc' d doc rest | isEmptyDoc doc = (d, Nothing) : rest
finishedDoc' d doc rest | notDocDecl d   = (d, Just doc) : rest
  where
    notDocDecl (L _ (DocD _)) = False
    notDocDecl _              = True
finishedDoc' _ _ rest = rest
