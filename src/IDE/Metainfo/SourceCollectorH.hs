{-# OPTIONS_GHC -XScopedTypeVariables -XBangPatterns #-}
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
,   interfaceToModuleDescr
,   PackageCollectStats(..)
) where

import IDE.Core.CTypes
       (getThisPackage, PackageDescr(..), TypeDescr(..), RealDescr(..),
        Descr(..), ModuleDescr(..), PackModule(..), SimpleDescr(..),
        packageIdentifierToString)

#ifdef MIN_VERSION_haddock_leksah
import Haddock.Types
       (ExportItem(..), DeclInfo,
        Interface(..))
import Haddock.Interface
#else
import Documentation.Haddock
#endif
import Distribution.Text (simpleParse)
#if MIN_VERSION_ghc(7,6,0)
import InstEnv (ClsInst(..))
#else
import InstEnv (Instance(..))
#endif
import MyMissing
import Data.Map (Map)
import qualified Data.Map as Map (empty)

import Data.List (nub)
import qualified Data.ByteString.Char8 as BS (pack)
#if MIN_VERSION_ghc(6,12,1)
import IDE.Metainfo.WorkspaceCollector
       (srcSpanToLocation, uncommentDecl, uncommentData, printHsDoc, sortByLoc)
#else
import IDE.Metainfo.WorkspaceCollector
       (srcSpanToLocation, uncommentDecl, uncommentData, sortByLoc)
#endif

import Name (getOccString,getSrcSpan)
import PackageConfig (PackageConfig)
import Distribution.Verbosity (verbose)
import qualified Distribution.InstalledPackageInfo as IPI
import IDE.StrippedPrefs (getUnpackDirectory, Prefs(..))
import IDE.Metainfo.SourceDB (sourceForPackage, getSourcesMap)
import MonadUtils (liftIO)
import System.Directory (setCurrentDirectory, doesDirectoryExist,createDirectory)
import System.FilePath ((<.>), dropFileName, (</>))
import Data.Maybe(mapMaybe)
import IDE.Utils.GHCUtils (inGhcIO)
import qualified Control.Exception as NewException (SomeException, catch)
import IDE.Utils.Tool
import Control.Monad (unless)
import IDE.Utils.FileUtils (figureOutGhcOpts, myCanonicalizePath)
import Distribution.Package(PackageIdentifier)
import GHC hiding(Id,Failed,Succeeded,ModuleName)
import System.Log.Logger (warningM, debugM)
import Control.DeepSeq (deepseq)
import Data.ByteString.Char8 (ByteString)
#if MIN_VERSION_ghc(7,6,0)
import Outputable hiding(trace)
#else
import Outputable hiding(trace, showSDoc, showSDocUnqual)
import qualified Outputable as O
#endif
import GHC.Show(showSpace)
import Name

#ifdef MIN_VERSION_haddock_leksah
#else
type HsDoc = Doc
#endif

type NDoc  = HsDoc Name

isEmptyDoc :: NDoc -> Bool
isEmptyDoc DocEmpty  = True
isEmptyDoc _         = False

#if MIN_VERSION_ghc(6,12,1)
type MyLDocDecl = LDocDecl
#else
type MyLDocDecl = LDocDecl Name
#endif

#if !MIN_VERSION_ghc(7,6,0)
showSDoc :: DynFlags -> SDoc -> String
showSDoc _ = O.showSDoc
showSDocUnqual :: DynFlags -> SDoc -> String
showSDocUnqual _ = O.showSDocUnqual
#endif

show' :: Outputable alpha => DynFlags -> alpha  -> String
show' dflags = showSDoc dflags . ppr

data PackageCollectStats = PackageCollectStats {
    packageString       :: String,
    modulesTotal        :: Maybe Int,
    withSource          :: Bool,
    retrieved           :: Bool,
    mbError             :: Maybe String}

findSourceForPackage :: Prefs -> PackageConfig -> IO (Either String FilePath)
findSourceForPackage prefs packageConfig = do
    sourceMap <- liftIO $ getSourcesMap prefs
    case sourceForPackage (getThisPackage packageConfig) sourceMap of
        Just fpSource -> return (Right fpSource)
        Nothing -> do
            unpackDir <- getUnpackDirectory prefs
            case unpackDir of
                Nothing -> return (Left "No source found. Prefs don't allow for retreiving")
                Just fpUnpack -> do
                    exists <- doesDirectoryExist fpUnpack
                    unless exists $ createDirectory fpUnpack
                    setCurrentDirectory fpUnpack
                    runTool' "cabal" (["unpack",packageName]) Nothing
                    success <- doesDirectoryExist (fpUnpack </> packageName)
                    if not success
                        then return (Left "Failed to download and unpack source")
                        else return (Right (fpUnpack </> packageName </>  takeWhile (/= '-') packageName <.> "cabal"))
    where
        packageName = packageIdentifierToString (getThisPackage packageConfig)


packageFromSource :: FilePath -> PackageConfig -> IO (Maybe PackageDescr, PackageCollectStats)
packageFromSource cabalPath packageConfig = do
    setCurrentDirectory dirPath
    ghcFlags <- figureOutGhcOpts
    debugM "leksah-server" ("ghcFlags:  " ++ show ghcFlags)	
    NewException.catch (inner ghcFlags) handler
    where
        _handler' (_e :: NewException.SomeException) = do
            debugM "leksah-server" "would block"
            return ([])
        handler (e :: NewException.SomeException) = do
            warningM "leksah-server" ("Ghc failed to process: " ++ show e)
            return (Nothing, PackageCollectStats packageName Nothing False False
                                            (Just ("Ghc failed to process: " ++ show e)))
        inner ghcFlags = inGhcIO ghcFlags [Opt_Haddock] $ \ dflags -> do
#if MIN_VERSION_haddock(2,8,0)
            (interfaces,_) <- processModules verbose (exportedMods ++ hiddenMods) [] []
#else
            (interfaces,_) <- createInterfaces verbose (exportedMods ++ hiddenMods) [] []
#endif
            liftIO $ print (length interfaces)
            let mods = map (interfaceToModuleDescr dflags dirPath (getThisPackage packageConfig)) interfaces
            sp <- liftIO $ myCanonicalizePath dirPath
            let pd = PackageDescr {
                    pdPackage           =   getThisPackage packageConfig
                ,   pdModules           =   mods
                ,   pdBuildDepends      =   [] -- TODO depends packageConfig
                ,   pdMbSourcePath      =   Just sp}
            let stat = PackageCollectStats packageName (Just (length mods)) True False Nothing
            liftIO $ deepseq pd $ return (Just pd, stat)
        exportedMods = map moduleNameString $ IPI.exposedModules packageConfig
        hiddenMods   = map moduleNameString $ IPI.hiddenModules packageConfig
        dirPath      = dropFileName cabalPath
        packageName  = packageIdentifierToString (getThisPackage packageConfig)

-- Heaven

interfaceToModuleDescr :: DynFlags -> FilePath -> PackageIdentifier -> Interface -> ModuleDescr
interfaceToModuleDescr dflags _dirPath pid interface =
    ModuleDescr {
        mdModuleId          =   PM pid modName
    ,   mdMbSourcePath      =   Just filepath
    ,   mdReferences        =   imports
    ,   mdIdDescriptions    =   descrs}
    where
        filepath   = ifaceOrigFilename interface
        modName    = forceJust ((simpleParse . moduleNameString . moduleName . ifaceMod) interface)
                        "Can't parse module name"
        descrs     = extractDescrs dflags (PM pid modName)
                        (ifaceDeclMap interface) (ifaceExportItems interface)
                        (ifaceInstances interface) [] --(ifaceLocals interface)
        imports    = Map.empty --TODO

#if MIN_VERSION_ghc(7,6,0)
getDoc :: Documentation Name -> Maybe NDoc
getDoc = documentationDoc
#else
getDoc :: Maybe NDoc -> Maybe NDoc
getDoc = id
#endif

#if MIN_VERSION_ghc(7,4,1)
type DeclInfo = [LHsDecl Name]
#endif
#if MIN_VERSION_ghc(6,12,1)
#if MIN_VERSION_ghc(7,6,0)
extractDescrs :: DynFlags -> PackModule -> Map Name DeclInfo -> [ExportItem Name] -> [ClsInst] -> [Name] -> [Descr]
#else
extractDescrs :: DynFlags -> PackModule -> Map Name DeclInfo -> [ExportItem Name] -> [Instance] -> [Name] -> [Descr]
#endif
extractDescrs dflags pm _ifaceDeclMap ifaceExportItems' ifaceInstances' _ifaceLocals =
        transformToDescrs dflags pm exportedDeclInfo ++ map (toDescrInst dflags pm) ifaceInstances'
    where
        exportedDeclInfo                               =  mapMaybe toDeclInfo  ifaceExportItems'
        toDeclInfo (ExportDecl decl mbDoc subDocs _)   =
                                        Just(decl,getDoc $ fst mbDoc,map (\ (a,b) -> (a,getDoc $ fst b)) subDocs)
        toDeclInfo (ExportNoDecl _ _)                  = Nothing
        toDeclInfo (ExportGroup _ _ _)                 = Nothing
        toDeclInfo (ExportDoc _)                       = Nothing
        toDeclInfo (ExportModule _)                    = Nothing
#else
extractDescrs :: DynFlags -> PackModule -> Map Name DeclInfo -> [ExportItem Name] -> [Instance] -> [Name] -> [Descr]
extractDescrs dflags pm _ifaceDeclMap ifaceExportItems' ifaceInstances' _ifaceLocals =
        transformToDescrs dflags pm exportedDeclInfo ++ map (toDescrInst dflags pm) ifaceInstances'
    where
        exportedDeclInfo                               =  mapMaybe toDeclInfo  ifaceExportItems'
        toDeclInfo (ExportDecl decl mbDoc subDocs _)   = Just(decl,mbDoc,subDocs)
        toDeclInfo (ExportNoDecl _ _)                  = Nothing
        toDeclInfo (ExportGroup _ _ _)                 = Nothing
        toDeclInfo (ExportDoc _)                       = Nothing
        toDeclInfo (ExportModule _)                    = Nothing
#endif

transformToDescrs :: DynFlags -> PackModule -> [(LHsDecl Name, Maybe NDoc, [(Name, Maybe NDoc)])] -> [Descr]
transformToDescrs dflags pm = concatMap transformToDescr
    where
#if MIN_VERSION_ghc(7,2,0)
    transformToDescr ((L loc (SigD (TypeSig [name] typ))), mbComment,_subCommentList) =
#else
    transformToDescr ((L loc (SigD (TypeSig name typ))), mbComment,_subCommentList) =
#endif
        [Real $ RealDescr {
        dscName'        =   getOccString (unLoc name)
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr typ))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment dflags mbComment []
    ,   dscTypeHint'    =   VariableDescr
    ,   dscExported'    =   True}]

    transformToDescr ((L _loc (SigD _)), _mbComment, _subCommentList) = []

#if MIN_VERSION_ghc(7,6,0)
    transformToDescr ((L loc (TyClD typ@(ForeignType {tcdLName = lid}))), mbComment,_sigList) =
        [Real $ RealDescr {
        dscName'        =   getOccString (unLoc lid)
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr typ))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment dflags mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]

    transformToDescr ((L loc (TyClD typ@(TyFamily {tcdLName = lid}))), mbComment,_sigList) =
        [Real $ RealDescr {
        dscName'        =   getOccString (unLoc lid)
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr typ))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment dflags mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]
#endif

#if MIN_VERSION_ghc(7,6,0)
    transformToDescr ((L loc (TyClD typ@(TyDecl {tcdLName = lid, tcdTyDefn = TySynonym {}}))), mbComment,_sigList) =
#else
    transformToDescr ((L loc (TyClD typ@(TySynonym lid _ _ _ ))), mbComment, _subCommentList) =
#endif
        [Real $ RealDescr {
        dscName'        =   getOccString (unLoc lid)
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr typ))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment dflags mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]

#if MIN_VERSION_ghc(7,6,0)
    transformToDescr ((L loc (TyClD typ@(TyDecl {tcdLName = lid, tcdTyDefn = TyData {td_cons=lConDecl, td_derivs=tcdDerivs'}}))), mbComment,_sigList) =
#else
    transformToDescr ((L loc (TyClD typ@(TyData DataType _ lid _ _ _ lConDecl tcdDerivs'))), mbComment,_subCommentList) =
#endif
        [Real $ RealDescr {
        dscName'        =   name
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr (uncommentData typ)))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment dflags mbComment []
    ,   dscTypeHint'    =   DataDescr constructors fields
    ,   dscExported'    =   True}]
            ++ derivings tcdDerivs'
        where
        constructors    =   map (extractConstructor dflags) lConDecl
        fields          =   nub $ concatMap (extractRecordFields dflags) lConDecl
        name            =   getOccString (unLoc lid)
        derivings Nothing = []
        derivings (Just _l) = []

#if !MIN_VERSION_ghc(7,6,0)
    transformToDescr ((L loc (TyClD typ@(TyData NewType _ tcdLName' _ _ _ lConDecl tcdDerivs'))), mbComment,_subCommentList) =
        [Real $ RealDescr {
        dscName'        =   name
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr (uncommentData typ)))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment dflags mbComment []
    ,   dscTypeHint'    =   NewtypeDescr constructor mbField
    ,   dscExported'    =   True}]
        ++ derivings tcdDerivs'
        where
        constructor     =   forceHead (map (extractConstructor dflags) lConDecl)
                                "WorkspaceCollector>>transformToDescr: no constructor for newtype"
        mbField         =   case concatMap (extractRecordFields dflags) lConDecl of
                                [] -> Nothing
                                a:_ -> Just a
        name            =   getOccString (unLoc tcdLName')
        derivings Nothing = []
        derivings (Just _l) = []
#endif

    transformToDescr ((L loc (TyClD cl@(ClassDecl{tcdLName=tcdLName', tcdSigs=tcdSigs', tcdDocs=docs}))), mbComment,_subCommentList) =
        [Real $ RealDescr {
        dscName'        =   getOccString (unLoc tcdLName')
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr cl{tcdMeths = emptyLHsBinds}))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment dflags mbComment []
    ,   dscTypeHint'    =   ClassDescr super methods
    ,   dscExported'    =   True    }]
        where
        methods         =   extractMethods dflags tcdSigs' docs
        super           =   []

    transformToDescr (_, _mbComment, _sigList) = []

#if MIN_VERSION_ghc(7,6,0)
toDescrInst :: DynFlags -> PackModule -> ClsInst -> Descr
toDescrInst dflags pm inst@(ClsInst {is_cls = is_cls', is_tys = is_tys'}) =
#else
toDescrInst :: DynFlags -> PackModule -> Instance -> Descr
toDescrInst dflags pm inst@(Instance is_cls' _is_tcs _is_tvs is_tys' _is_dfun _is_flag) =
#endif
        Real $ RealDescr {
        dscName'        =   getOccString is_cls'
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr inst))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation (getSrcSpan inst)
    ,   dscMbComment'   =   Nothing
    ,   dscTypeHint'    =   InstanceDescr (map (showSDocUnqual dflags . ppr) is_tys')
    ,   dscExported'    =   True}

extractMethods :: DynFlags -> [LSig Name] -> [MyLDocDecl] -> [SimpleDescr]
extractMethods dflags sigs docs =
    let pairs = attachComments' dflags sigs docs
    in mapMaybe (extractMethod dflags) pairs

extractMethod :: DynFlags -> (LHsDecl Name, Maybe NDoc) -> Maybe SimpleDescr
#if MIN_VERSION_ghc(7,2,0)
extractMethod dflags ((L loc (SigD ts@(TypeSig [name] _typ))), mbDoc) =
#else
extractMethod dflags ((L loc (SigD ts@(TypeSig name _typ))), mbDoc) =
#endif
    Just $ SimpleDescr
        (getOccString (unLoc name))
        (Just (BS.pack (showSDocUnqual dflags $ ppr ts)))
        (srcSpanToLocation loc)
        (toComment dflags mbDoc [])
        True
extractMethod _dflags (_, _mbDoc) = Nothing

extractConstructor :: DynFlags -> LConDecl Name -> SimpleDescr
extractConstructor dflags decl@(L loc (ConDecl {con_name = name, con_doc = doc})) =
    SimpleDescr
        (getOccString (unLoc name))
        (Just (BS.pack (showSDocUnqual dflags $ppr (uncommentDecl decl))))
        (srcSpanToLocation loc)
        (case doc of
            Nothing -> Nothing
            Just (L _ d) -> Just (BS.pack (printHsDoc d)))
        True

extractRecordFields :: DynFlags -> LConDecl Name -> [SimpleDescr]
extractRecordFields dflags (L _ _decl@(ConDecl {con_details=(RecCon flds)})) =
    map extractRecordFields' flds
    where
    extractRecordFields' _field@(ConDeclField (L loc name) typ doc) =
        SimpleDescr
            (getOccString name)
            (Just (BS.pack (showSDocUnqual dflags $ ppr typ)))
            (srcSpanToLocation loc)
            (case doc of
                Nothing -> Nothing
                Just (L _ d) -> Just (BS.pack (printHsDoc d)))
            True
extractRecordFields _ _ = []

toComment :: DynFlags -> Maybe NDoc -> [NDoc] -> Maybe ByteString
toComment dflags (Just c) _    =  Just (BS.pack (printHsDoc' dflags c))
toComment dflags Nothing (c:_) =  Just (BS.pack (printHsDoc' dflags c))
toComment _ Nothing []         =  Nothing


{--
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [Data] []
collectParseInfoForDecl (l,st) ((Just (L loc (TyClD (TyFamily _ lid _ _)))), mbComment')
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [] []
collectParseInfoForDecl (l,st) ((Just (L loc (TyClD (ClassDecl _ lid _ _ _ _ _ _ )))), mbComment')
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [Class] []
--}

printHsDoc' :: DynFlags -> HsDoc Name  -> String
printHsDoc' dflags d = show (PPDoc dflags d)

data PPDoc alpha = PPDoc DynFlags (HsDoc alpha)

instance Outputable alpha => Show (PPDoc alpha)  where
    showsPrec _ (PPDoc _ DocEmpty)                 =   id
    showsPrec _ (PPDoc d (DocAppend l r))          =   shows (PPDoc d l)  . shows (PPDoc d r)
    showsPrec _ (PPDoc _ (DocString str))          =   showString str
    showsPrec _ (PPDoc d (DocParagraph doc))         =   shows (PPDoc d doc) . showChar '\n'
    showsPrec _ (PPDoc d (DocIdentifier l))        =   foldr (\i _f -> showChar '\'' .
                                                     ((showString . showSDoc d .  ppr) i) . showChar '\'') id [l]
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
#if MIN_VERSION_ghc(7,6,0)
    showsPrec _ (PPDoc _ (DocHyperlink h))            =   showChar '<' . showString (show h) . showChar '>'
#else
    showsPrec _ (PPDoc _ (DocURL str))            =   showChar '<' . showString str . showChar '>'
#endif
    showsPrec _ (PPDoc _ (DocAName str))          =   showChar '#' . showString str . showChar '#'
    showsPrec _ (PPDoc _ _)                       =   id

attachComments' :: DynFlags -> [LSig Name] -> [MyLDocDecl] -> [(LHsDecl Name, Maybe (HsDoc Name))]
attachComments' dflags sigs docs = collectDocs' dflags $ sortByLoc $
        ((map (\ (L l i) -> L l (SigD i)) sigs) ++ (map (\ (L l i) -> L l (DocD i)) docs))

-- | Collect the docs and attach them to the right declaration.
collectDocs' :: DynFlags -> [LHsDecl Name] -> [(LHsDecl Name, (Maybe (HsDoc Name)))]
collectDocs' dflags = collect' dflags Nothing DocEmpty

collect' :: DynFlags -> Maybe (LHsDecl Name) -> HsDoc Name -> [LHsDecl Name] -> [(LHsDecl Name, (Maybe (HsDoc Name)))]
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

finishedDoc' :: LHsDecl alpha -> NDoc -> [(LHsDecl alpha, (Maybe ((HsDoc Name))))]
                    -> [(LHsDecl alpha, (Maybe ((HsDoc Name))))]
finishedDoc' d doc rest | isEmptyDoc doc = (d, Nothing) : rest
finishedDoc' d doc rest | notDocDecl d   = (d, Just doc) : rest
  where
    notDocDecl (L _ (DocD _)) = False
    notDocDecl _              = True
finishedDoc' _ _ rest = rest
