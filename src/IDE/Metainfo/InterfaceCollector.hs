{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Metainfo.InterfaceCollector
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- | This modulle extracts information from .hi files for installed packages
--
-------------------------------------------------------------------------------

module IDE.Metainfo.InterfaceCollector (
    collectPackageFromHI
,   extractExportedDescrH
,   extractExportedDescrR
) where

import MyMissing (nonEmptyLines)
#if MIN_VERSION_ghc(7,10,0)
import Module hiding (PackageKey, ModuleName)
#else
import Module hiding (PackageId, ModuleName)
#endif
import qualified Module as Module (ModuleName)
import qualified Maybes as M
import DynFlags (DynFlags)
#if MIN_VERSION_ghc(7,2,0)
import HscTypes
import GhcMonad hiding (liftIO)
import qualified GhcMonad as Hs (liftIO)
#else
import HscTypes hiding (liftIO)
import qualified HscTypes as Hs (liftIO)
#endif
#if MIN_VERSION_ghc(8,0,0)
import Avail
import TysWiredIn ( )
#elif MIN_VERSION_ghc(7,3,0)
import Avail
import TysWiredIn ( eqTyConName )
#endif
import LoadIface
#if MIN_VERSION_ghc(7,6,0)
import Outputable hiding(trace)
#else
import Outputable hiding(trace, showSDoc, showSDocUnqual)
import qualified Outputable as O
#endif
import IfaceSyn
import FastString
import Name
import PrelNames
import PackageConfig (PackageConfig)
import TcRnTypes
import Data.Char (isSpace)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
#if MIN_VERSION_ghc(8,2,0)
import ToIface (toIfaceTyCon_name)
import FieldLabel (flSelector)
import GHC.PackageDb (exposedModules, hiddenModules)
#elif MIN_VERSION_ghc(7,10,0)
import GHC.PackageDb (exposedModules, hiddenModules, exposedName)
#else
import PackageConfig (mkPackageId)
import qualified Distribution.InstalledPackageInfo as IPI
#endif
import Distribution.Package hiding (PackageId)
import Distribution.ModuleName
import Distribution.Text (simpleParse)
import qualified Data.ByteString.Char8 as BS
import IDE.Core.Serializable ()
import IDE.Core.CTypes
import Data.ByteString.Char8 (ByteString)
import TcRnMonad (initTcRnIf)
import IDE.Utils.GHCUtils
import IDE.Utils.FileUtils (getSysLibDir)
import Control.DeepSeq(deepseq)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import System.Log.Logger (debugM)

#if MIN_VERSION_ghc(8,2,0)
exposedName :: (Module.ModuleName, Maybe Module.Module) -> Module.ModuleName
exposedName = fst
nameOccName82 :: Name -> OccName
nameOccName82 = nameOccName
#else
mkPackageName :: String -> PackageName
mkPackageName = PackageName
nameOccName82 :: OccName -> OccName
nameOccName82 = id
flSelector :: OccName -> OccName
flSelector = id
ifConName :: IfaceConDecl -> OccName
ifConName = ifConOcc
#endif

#if !MIN_VERSION_ghc(7,6,0)
showSDoc :: DynFlags -> SDoc -> Text
showSDoc _ = O.showSDoc
showSDocUnqual :: DynFlags -> SDoc -> Text
showSDocUnqual _ = O.showSDocUnqual
#endif

collectPackageFromHI :: PackageConfig -> [FilePath] -> IO PackageDescr
collectPackageFromHI packageConfig dbs = do
  libDir <- getSysLibDir VERSION_ghc
  inGhcIO libDir [] [] dbs $ \ dflags -> do
    let pIdAndKey = getThisPackage packageConfig
    Hs.liftIO . debugM "leksah-server" $ "collectPackageFromHI"
    session             <-  getSession
    exportedIfaceInfos  <-  getIFaceInfos pIdAndKey
#if MIN_VERSION_ghc(7,10,0)
                                            (map exposedName $ exposedModules packageConfig) session
#else
                                            (IPI.exposedModules packageConfig) session
#endif
    hiddenIfaceInfos    <-  getIFaceInfos pIdAndKey
#if MIN_VERSION_ghc(7,10,0)
                                            (hiddenModules packageConfig) session
#else
                                            (IPI.hiddenModules packageConfig) session
#endif
    let pd = extractInfo dflags exportedIfaceInfos hiddenIfaceInfos pIdAndKey
                                            [] -- TODO 6.12 (IPI.depends $ packageConfigToInstalledPackageInfo packageConfig))
    deepseq pd (return pd)


getIFaceInfos :: PackageIdAndKey -> [Module.ModuleName] -> HscEnv -> Ghc [(ModIface, FilePath)]
getIFaceInfos p modules _session = do
    let pid             =   packId p
#if MIN_VERSION_ghc(8,2,0)
        makeMod         =   mkModule (DefiniteUnitId (DefUnitId (packUnitId p)))
        makeInstMod     =   InstalledModule (packUnitId p)
#elif MIN_VERSION_ghc(8,0,0)
        makeMod         =   mkModule (packUnitId p)
#elif MIN_VERSION_ghc(7,10,0)
        makeMod         =   mkModule (packKey p)
#else
        makeMod         =   mkModule (mkPackageId pid)
#endif
        isBase          =   pkgName pid == mkPackageName "base"
        ifaces          =   mapM (\ mn -> findAndReadIface empty
#if MIN_VERSION_ghc(8,2,0)
                                          (makeInstMod mn)
#endif
                                          (if isBase
                                                then mkBaseModule_ mn
                                                else makeMod mn)
                                          False) modules
    hscEnv              <-  getSession
    let gblEnv          =   IfGblEnv { if_rec_types = Nothing, if_doc = empty }
    maybes              <-  Hs.liftIO $ initTcRnIf  'i' hscEnv gblEnv () ifaces
    let res             =   mapMaybe handleErr maybes
    return res
    where
        handleErr (M.Succeeded val)   =   Just val
        handleErr (M.Failed _mess)    =   Nothing

-------------------------------------------------------------------------

converModuleName :: Module.ModuleName -> ModuleName
converModuleName = fromJust . simpleParse . moduleNameString

extractInfo :: DynFlags -> [(ModIface, FilePath)] -> [(ModIface, FilePath)] -> PackageIdAndKey ->
                    [PackageIdentifier] -> PackageDescr
extractInfo dflags ifacesExp ifacesHid pid buildDepends =
    let allDescrs           =   concatMap (extractExportedDescrH dflags pid . fst)
                                  (ifacesHid ++ ifacesExp)
        mods                =   map (extractExportedDescrR dflags pid allDescrs . fst) ifacesExp
    in PackageDescr {
        pdPackage           =   packId pid
    ,   pdModules           =   mods
    ,   pdBuildDepends      =   buildDepends
    ,   pdMbSourcePath      =   Nothing}

extractExportedDescrH :: DynFlags -> PackageIdAndKey -> ModIface -> [((ModuleName, OccName), Descr)]
extractExportedDescrH dflags pid iface =
    let mid                 =   converModuleName . moduleName $ mi_module iface
        exportedNames       =   Set.fromList
                                $ map nameOccName
                                    $ concatMap availNames
                                        $ mi_exports iface
        exportedDecls       =   filter (\ ifdecl -> nameOccName82 (ifName ifdecl) `Set.member` exportedNames)
                                                            (map snd (mi_decls iface))
    in  concatMap (extractIdentifierDescr dflags pid mid) exportedDecls


extractExportedDescrR :: DynFlags
    -> PackageIdAndKey
    -> [((ModuleName, OccName), Descr)]
    -> ModIface
    -> ModuleDescr
extractExportedDescrR dflags pid hidden iface =
    let mid             =   converModuleName . moduleName $ mi_module iface
        exportedNames   =   Set.fromList
                                $ map (\n -> (converModuleName . moduleName $ nameModule n, nameOccName n))
                                    $ concatMap availNames
                                        $ mi_exports iface
        exportedDecls   =   filter (\ ifdecl -> (converModuleName . moduleName $ mi_module iface, nameOccName82 (ifName ifdecl))
                                                    `Set.member` exportedNames)
                                                            (map snd (mi_decls iface))
        ownDecls        =   concatMap (extractIdentifierDescr dflags pid mid) exportedDecls
        otherDecls      =   exportedNames `Set.difference` Set.fromList (map fst ownDecls)
        reexported      =   map (Reexported . ReexportedDescr (Just (PM (packId pid) mid)) . snd)
                                 $ filter (\k -> fst k `Set.member` otherDecls) hidden
        inst            =   concatMap (extractInstances dflags (PM (packId pid) mid)) (mi_insts iface)
        uses            =   Map.fromList . catMaybes $ map (extractUsages dflags) (mi_usages iface)
        declsWithExp    =   map withExp ownDecls
        withExp (n, Real d) =  Real $ d{dscExported' = Set.member n exportedNames}
        withExp _        =  error "Unexpected Reexported"
    in  ModuleDescr {
                    mdModuleId          =   PM (packId pid) mid
                ,   mdMbSourcePath      =   Nothing
                ,   mdReferences        =   uses
                ,   mdIdDescriptions    =   declsWithExp ++ inst ++ reexported}

extractIdentifierDescr :: DynFlags -> PackageIdAndKey -> ModuleName -> IfaceDecl -> [((ModuleName, OccName), Descr)]
extractIdentifierDescr dflags package mid decl
   =    let descr = RealDescr{
                    dscName'           =   T.pack . unpackFS . occNameFS . nameOccName82 $ ifName decl
                ,   dscMbTypeStr'      =   Just . BS.pack . unlines . nonEmptyLines . filterExtras . showSDocUnqual dflags $ ppr decl
                ,   dscMbModu'         =   Just (PM (packId package) mid)
                ,   dscMbLocation'     =   Nothing
                ,   dscMbComment'      =   Nothing
                ,   dscTypeHint'       =   VariableDescr
                ,   dscExported'       =   True
                }
        in map ((mid, nameOccName82 $ ifName decl),) $ case decl of
            IfaceId{} -> [Real descr]
            IfaceData{ifName = name, ifCons = ifCons'}
                -> let d = case ifCons' of
                            IfDataTyCon {}
                                ->  let
                                        fieldNames          =   concatMap (extractFields dflags) (visibleIfConDecls ifCons')
                                        constructors'       =   extractConstructors dflags (nameOccName82 name) (visibleIfConDecls ifCons')
                                    in DataDescr constructors' fieldNames
                            IfNewTyCon {}
                                ->  let
                                        fieldNames          =   concatMap (extractFields dflags) (visibleIfConDecls ifCons')
                                        constructors'       =   extractConstructors dflags (nameOccName82 name) (visibleIfConDecls ifCons')
                                        mbField             =   case fieldNames of
                                                                    [] -> Nothing
                                                                    [fn] -> Just fn
                                                                    _ -> error $ "InterfaceCollector >> extractIdentifierDescr: "
                                                                         ++ "Newtype with more then one field"
                                        constructor         =   case constructors' of
                                                                    [c] -> c
                                                                    _ -> error $ "InterfaceCollector >> extractIdentifierDescr: "
                                                                         ++ "Newtype with not exactly one constructor"
                                    in NewtypeDescr constructor mbField
#if MIN_VERSION_ghc(7,3,0)
                            IfAbstractTyCon {} ->  DataDescr [] []
#else
                            IfAbstractTyCon ->  DataDescr [] []
#endif
#if MIN_VERSION_ghc(8,0,0)
#elif MIN_VERSION_ghc(7,6,0)
                            IfDataFamTyCon ->  DataDescr [] []
#else
                            IfOpenDataTyCon ->  DataDescr [] []
#endif
                    in [Real (descr{dscTypeHint' = d})]
#if MIN_VERSION_ghc(8,2,0)
            IfaceClass{ifBody = IfAbstractClass}
                        ->  let
                                classOpsID          =   []
                                superclasses        =   []
                            in [Real descr{dscTypeHint' = ClassDescr superclasses classOpsID}]
            IfaceClass{ifBody = IfConcreteClass{ifClassCtxt = context, ifSigs = ifSigs'}}
#elif MIN_VERSION_ghc(7,6,0)
            IfaceClass{ifCtxt = context, ifSigs = ifSigs'}
#else
            (IfaceClass context _ _ _ _ ifSigs' _ )
#endif
                        ->  let
                                classOpsID          =   map (extractClassOp dflags) ifSigs'
                                superclasses        =   extractSuperClassNames context
                            in [Real descr{dscTypeHint' = ClassDescr superclasses classOpsID}]
#if MIN_VERSION_ghc(7,6,0)
            IfaceAxiom {}
                        ->  [Real descr]
#endif
#if MIN_VERSION_ghc(7,10,0)
            IfaceSynonym {}
                        ->  [Real $ descr{dscTypeHint' = TypeDescr}]
            IfaceFamily {}
                        ->  [Real $ descr{dscTypeHint' = TypeDescr}]
#else
            IfaceSyn {}
                        ->  [Real $ descr{dscTypeHint' = TypeDescr}]
            IfaceForeign{}
                        ->  [Real descr]
#endif
#if MIN_VERSION_ghc(7,8,0)
            IfacePatSyn {}
                        ->  [Real descr{dscTypeHint' = PatternSynonymDescr}]
#endif

extractConstructors :: DynFlags -> OccName -> [IfaceConDecl] -> [SimpleDescr]
extractConstructors dflags name = map (\decl -> SimpleDescr (T.pack . unpackFS $occNameFS (nameOccName82 $ ifConName decl))
                                                 (Just (BS.pack $ filterExtras $ showSDocUnqual dflags $
#if MIN_VERSION_ghc(7,10,0)
                                                    pprIfaceForAllPart (ifConExTvs decl)
#else
                                                    pprIfaceForAllPart (ifConUnivTvs decl ++ ifConExTvs decl)
#endif
                                                        (eq_ctxt decl ++ ifConCtxt decl) (pp_tau decl)))
                                                 Nothing Nothing True)

    where
    pp_tau decl     = case map pprParendIfaceType (ifConArgTys decl) ++ [pp_res_ty decl] of
                                (t:ts) -> fsep (t : map (arrow <+>) ts)
                                []     -> panic "pp_con_taus"
#if MIN_VERSION_ghc(7,10,0)
    pp_res_ty _decl  = ppr name <+> fsep [] -- TODO figure out what to do here
#else
    pp_res_ty decl  = ppr name <+> fsep [ppr tv | (tv,_) <- ifConUnivTvs decl]
#endif
#if MIN_VERSION_ghc(8,2,0)
    eq_ctxt decl    = [IfaceTyConApp (toIfaceTyCon_name eqTyConName) (ITC_Vis (IfaceTyVar tv) (ITC_Vis ty ITC_Nil))
#elif MIN_VERSION_ghc(8,0,0)
    eq_ctxt decl    = [IfaceTyConApp (IfaceTyCon eqTyConName NoIfaceTyConInfo) (ITC_Vis (IfaceTyVar tv) (ITC_Vis ty ITC_Nil))
#elif MIN_VERSION_ghc(7,10,0)
    eq_ctxt decl    = [IfaceTyConApp (IfaceTc eqTyConName) (ITC_Type (IfaceTyVar tv) (ITC_Type ty ITC_Nil))
#elif MIN_VERSION_ghc(7,3,0)
    eq_ctxt decl    = [IfaceTyConApp (IfaceTc eqTyConName) [(IfaceTyVar (occNameFS tv)), ty]
#else
    eq_ctxt decl    = [(IfaceEqPred (IfaceTyVar (occNameFS tv)) ty)
#endif
                                | (tv,ty) <- ifConEqSpec decl]

extractFields :: DynFlags -> IfaceConDecl -> [SimpleDescr]
extractFields dflags decl = map (\ (n, t) -> SimpleDescr n t Nothing Nothing True)
                                $ zip (map (extractFieldNames . nameOccName82 . flSelector) (ifConFields decl))
                                        (map (extractType dflags) (ifConArgTys decl))

extractType :: DynFlags -> IfaceType -> Maybe ByteString
extractType dflags it = Just ((BS.pack . filterExtras . showSDocUnqual dflags . ppr) it)

extractFieldNames :: OccName -> Text
extractFieldNames occName' = T.pack . unpackFS $occNameFS occName'

extractClassOp :: DynFlags -> IfaceClassOp -> SimpleDescr
extractClassOp dflags (IfaceClassOp occName' _dm ty) = SimpleDescr (T.pack . unpackFS . occNameFS $ nameOccName82 occName')
                                                (Just (BS.pack $ showSDocUnqual dflags (ppr ty)))
                                                Nothing Nothing True

extractSuperClassNames :: [IfacePredType] -> [Text]
extractSuperClassNames = mapMaybe extractSuperClassName
    where
#if !MIN_VERSION_ghc(7,3,0)
            extractSuperClassName (IfaceClassP name _)  =
                Just (unpackFS $occNameFS $ nameOccName name)
#endif
            extractSuperClassName _                     =   Nothing

extractInstances :: DynFlags
    -> PackModule
#if MIN_VERSION_ghc(7,6,0)
    -> IfaceClsInst
#else
    -> IfaceInst
#endif
    -> [Descr]
extractInstances dflags pm ifaceInst  =
    let className   =   showSDocUnqual dflags $ ppr $ ifInstCls ifaceInst
        dataNames   =   map (T.pack . showSDocUnqual dflags . ppr) . catMaybes $ ifInstTys ifaceInst
    in [Real RealDescr
                    {   dscName'         =   T.pack className
                    ,   dscMbTypeStr'    =   Nothing
                    ,   dscMbModu'       =   Just pm
                    ,   dscMbLocation'   =   Nothing
                    ,   dscMbComment'    =   Nothing
                    ,   dscTypeHint'     =   InstanceDescr dataNames
                    ,   dscExported'     =   False}]


extractUsages :: DynFlags -> Usage -> Maybe (ModuleName, Set Text)
extractUsages _ UsagePackageModule {usg_mod = usg_mod'} =
    let name    =   (fromJust . simpleParse . moduleNameString) (moduleName usg_mod')
    in Just (name, Set.fromList [])
extractUsages dflags UsageHomeModule {usg_mod_name = usg_mod_name', usg_entities = usg_entities'} =
    let name    =   (fromJust . simpleParse . moduleNameString) usg_mod_name'
        ids     =   map (T.pack . showSDocUnqual dflags . ppr . fst) usg_entities'
    in Just (name, Set.fromList ids)
#if MIN_VERSION_ghc(7,4,0)
extractUsages _ UsageFile {} = Nothing
#endif
#if MIN_VERSION_ghc(8,2,0)
extractUsages _ UsageMergedRequirement {} = Nothing
#endif

filterExtras, filterExtras' :: String -> String
filterExtras ('{':'-':r)                =   filterExtras' r
filterExtras ('R':'e':'c':'F':'l':'a':'g':r)
                                        =   filterExtras (skipNextWord r)
filterExtras ('G':'e':'n':'e':'r':'i':'c':'s':':':r)
                                        =   filterExtras (skipNextWord r)
filterExtras ('F':'a':'m':'i':'l':'y':'I':'n':'s':'t':'a':'n':'c':'e':':':r)
                                        =   filterExtras (skipNextWord r)
filterExtras (c:r)                      =   c : filterExtras r
filterExtras []                         =   []

filterExtras' ('-':'}':r)   =   filterExtras r
filterExtras' (_:r)         =   filterExtras' r
filterExtras' []            =   []

skipNextWord, skipNextWord' :: String -> String
skipNextWord (a:r)
    | isSpace a             =   skipNextWord r
    | otherwise             =   skipNextWord' r
skipNextWord []             =   []

skipNextWord'(a:r)
        | a == '\n'          =   r
        | isSpace a         =   a:r
        | otherwise         =   skipNextWord' r
skipNextWord' []            =   []



