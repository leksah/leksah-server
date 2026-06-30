{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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

import Prelude ()
import Prelude.Compat
#if MIN_VERSION_ghc(9,0,0)
import GHC.Utils.Outputable (ppr, (<+>), fsep, arrow, empty)
import GHC.Driver.Env.KnotVars (emptyKnotVars)
import GHC.Utils.Panic (panic)
import GHC.CoreToIface (toIfaceTyCon_name)
import GHC.Builtin.Types (eqTyConName)
#if !MIN_VERSION_ghc(9,14,0)
import GHC.Types.Var (tyVarSpecToBinders)
#endif
import GHC.Types.FieldLabel (flSelector)
#if MIN_VERSION_ghc(9,12,0)
import GHC.Unit.Module.Location (ml_hi_file)
#endif
import GHC.Types.Name
import GHC.Unit.Module hiding (ModuleName, DefUnitId)
import GHC.Unit.Module.ModIface
import qualified GHC.Unit.Module as Module
import GHC.Unit.Info
import GHC.Driver.Env.Types
import GHC.Driver.Monad
#if MIN_VERSION_ghc(9,8,0)
import GHC.Driver.DynFlags
#else
import GHC.Driver.Session
#endif
import qualified GHC.Driver.Monad as Hs (liftIO)
import GHC.Tc.Types
import qualified GHC.Data.Maybe as M
import GHC.Iface.Syntax
import GHC.Core.UsageEnv hiding (Usage)
#else
import Module hiding (ModuleName)
import qualified Module (ModuleName)
import qualified Maybes as M
import DynFlags (DynFlags)
import HscTypes
import GhcMonad hiding (liftIO)
import qualified GhcMonad as Hs (liftIO)
#if MIN_VERSION_ghc(8,0,0)
import Avail
import TysWiredIn ( )
#else
import Avail
import TysWiredIn ( eqTyConName )
#endif
import LoadIface
import Outputable hiding(trace)
import IfaceSyn
import FastString
import Name
import PrelNames
import TcRnTypes
#if MIN_VERSION_ghc(8,2,0)
import ToIface (toIfaceTyCon_name)
import FieldLabel (flSelector)
import GHC.PackageDb (exposedModules, hiddenModules)
#else
import GHC.PackageDb (exposedModules, hiddenModules, exposedName)
#endif
import TcRnMonad (initTcRnIf)
import TysWiredIn
#endif

import Data.Char (isSpace)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import Distribution.Package hiding (PackageId)
import Distribution.ModuleName
import Distribution.Text (simpleParse)
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import Control.DeepSeq(deepseq)
import Data.Text (Text)
import qualified Data.Text as T (pack)
import System.Log.Logger (debugM)
import GHC.Stack (HasCallStack)

import IDE.Core.Serializable ()
import IDE.Core.CTypes
import IDE.Utils.GHCUtils
import IDE.Utils.FileUtils (getSysLibDir)
import IDE.Utils.Project (ProjectKey)
import GHC.Unit.Module.Deps
import GHC.Iface.Load (findAndReadIface)
import GHC.Tc.Utils.Monad (initTcRnIf)
import GHC.Types.Avail (availNames)
import GHC.Data.FastString (unpackFS)
import GHC.Driver.Ppr (showSDoc)

#if MIN_VERSION_ghc(8,2,0)
exposedName :: (Module.ModuleName, Maybe Module.Module) -> Module.ModuleName
exposedName = fst
nameOccName82 :: Name -> OccName
nameOccName82 = nameOccName
#else
nameOccName82 :: OccName -> OccName
nameOccName82 = id
flSelector :: OccName -> OccName
flSelector = id
ifConName :: IfaceConDecl -> OccName
ifConName = ifConOcc
#endif

#if MIN_VERSION_ghc(9,0,0)
-- | Since GHC 9.0 constructor argument types carry a multiplicity
-- (linear types); leksah only needs the type itself.
conArgIfaceTys :: IfaceConDecl -> [IfaceType]
conArgIfaceTys = map snd . ifConArgTys
#else
conArgIfaceTys :: IfaceConDecl -> [IfaceType]
conArgIfaceTys = ifConArgTys
#endif

#if MIN_VERSION_ghc(9,14,0)
-- | GHC 9.14 made `mi_usages` optional.
miUsages :: ModIface -> [Usage]
miUsages = fromMaybe [] . mi_usages
#else
miUsages :: ModIface -> [Usage]
miUsages = mi_usages
#endif

collectPackageFromHI :: HasCallStack => Maybe ProjectKey -> UnitInfo -> [FilePath] -> IO (Maybe PackageDescr)
collectPackageFromHI mbProject unitInfo dbs = do
  let pIdAndKey = getThisPackage unitInfo
  debugM "leksah-server" $ "collectPackageFromHI " ++ show (packId pIdAndKey) ++ " " ++ show dbs
  getSysLibDir mbProject (Just VERSION_ghc) >>= \case
    Nothing -> return Nothing
    Just libDir ->
      inGhcIO libDir [] [] dbs $ \ dflags -> do
        session             <-  getSession
        exportedIfaceInfos  <-  getIFaceInfos pIdAndKey
                                                (map exposedName $ unitExposedModules unitInfo) session
        hiddenIfaceInfos    <-  getIFaceInfos pIdAndKey
                                                (unitHiddenModules unitInfo) session
        let pd = extractInfo dflags exportedIfaceInfos hiddenIfaceInfos pIdAndKey
                                                [] -- TODO 6.12 (IPI.depends $ packageConfigToInstalledPackageInfo packageConfig))
        deepseq pd (return $ if null exportedIfaceInfos && null hiddenIfaceInfos then Nothing else Just pd)


getIFaceInfos :: PackageIdAndKey -> [Module.ModuleName] -> HscEnv -> Ghc [(ModIface, FilePath)]
getIFaceInfos p modules session = do
    let pid             =   packId p
#if MIN_VERSION_ghc(8,2,0)
        makeMod         =   mkModule (RealUnit (Definite (packUnitId p)))
        makeInstMod     =   mkModule (packUnitId p)
#elif MIN_VERSION_ghc(8,0,0)
        makeMod         =   mkModule (packUnitId p)
#else
        makeMod         =   mkModule (packKey p)
#endif
        isBase          =   pkgName pid == mkPackageName "base"
        ifaces          =   mapM (\ mn -> findAndReadIface session empty
#if MIN_VERSION_ghc(8,2,0)
                                          (makeInstMod mn)
#endif
                                          (makeMod mn)
                                          NotBoot) modules
    hscEnv              <-  getSession
    let gblEnv          =   IfGblEnv { if_rec_types = emptyKnotVars, if_doc = empty }
    maybes              <-  Hs.liftIO $ initTcRnIf  'i' hscEnv gblEnv () (Hs.liftIO ifaces)
    let res             =   mapMaybe handleErr maybes
    return res
    where
#if MIN_VERSION_ghc(9,12,0)
        handleErr (M.Succeeded (mi, ml)) = Just (mi, ml_hi_file ml)
#else
        handleErr (M.Succeeded val)   =   Just val
#endif
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
        uses            =   Map.fromList . catMaybes $ map (extractUsages dflags) (miUsages iface)
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
                ,   dscMbTypeStr'      =   Just . BS.pack . unlines . filter (any (not . isSpace)) . lines
                                                    . filterExtras . showSDoc dflags $ ppr decl
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
                            IfAbstractTyCon {} ->  DataDescr [] []
#if !MIN_VERSION_ghc(8,0,0)
                            IfDataFamTyCon ->  DataDescr [] []
#endif
                    in [Real (descr{dscTypeHint' = d})]
#if MIN_VERSION_ghc(8,2,0)
            IfaceClass{ifBody = IfAbstractClass}
                        ->  let
                                classOpsID          =   []
                                superclasses        =   []
                            in [Real descr{dscTypeHint' = ClassDescr superclasses classOpsID}]
            IfaceClass{ifBody = IfConcreteClass{ifClassCtxt = context, ifSigs = ifSigs'}}
#else
            IfaceClass{ifCtxt = context, ifSigs = ifSigs'}
#endif
                        ->  let
                                classOpsID          =   map (extractClassOp dflags) ifSigs'
                                superclasses        =   extractSuperClassNames context
                            in [Real descr{dscTypeHint' = ClassDescr superclasses classOpsID}]
            IfaceAxiom {}
                        ->  [Real descr]
            IfaceSynonym {}
                        ->  [Real $ descr{dscTypeHint' = TypeDescr}]
            IfaceFamily {}
                        ->  [Real $ descr{dscTypeHint' = TypeDescr}]
            IfacePatSyn {}
                        ->  [Real descr{dscTypeHint' = PatternSynonymDescr}]

extractConstructors :: DynFlags -> OccName -> [IfaceConDecl] -> [SimpleDescr]
extractConstructors dflags name = map (\decl -> SimpleDescr (T.pack . unpackFS $occNameFS (nameOccName82 $ ifConName decl))
                                                 (Just (BS.pack $ filterExtras $ showSDoc dflags $
-- GHC 9.14 changed `ifConUserTvBinders` to return `[IfaceForAllBndr]`
-- directly; 8.4–9.12 return specificity binders needing conversion.
#if MIN_VERSION_ghc(9,14,0)
                                                    pprIfaceForAllPart (ifConUserTvBinders decl)
#elif MIN_VERSION_ghc(8,4,0)
                                                    pprIfaceForAllPart (tyVarSpecToBinders (ifConUserTvBinders decl))
#else
                                                    pprIfaceForAllPart (ifConExTvs decl)
#endif
                                                        (eq_ctxt decl ++ ifConCtxt decl) (pp_tau decl)))
                                                 Nothing Nothing True)

    where
    pp_tau decl     = case map pprParendIfaceType (conArgIfaceTys decl) ++ [pp_res_ty decl] of
                                (t:ts) -> fsep (t : map (arrow <+>) ts)
                                []     -> panic "pp_con_taus"
    pp_res_ty _decl  = ppr name <+> fsep [] -- TODO figure out what to do here
#if MIN_VERSION_ghc(8,8,0)
    eq_ctxt decl    = [IfaceTyConApp (toIfaceTyCon_name eqTyConName) (IA_Arg (IfaceTyVar tv) Required (IA_Arg ty Required IA_Nil))
#elif MIN_VERSION_ghc(8,2,0)
    eq_ctxt decl    = [IfaceTyConApp (toIfaceTyCon_name eqTyConName) (ITC_Vis (IfaceTyVar tv) (ITC_Vis ty ITC_Nil))
#elif MIN_VERSION_ghc(8,0,0)
    eq_ctxt decl    = [IfaceTyConApp (IfaceTyCon eqTyConName NoIfaceTyConInfo) (ITC_Vis (IfaceTyVar tv) (ITC_Vis ty ITC_Nil))
#else
    eq_ctxt decl    = [IfaceTyConApp (IfaceTc eqTyConName) (ITC_Type (IfaceTyVar tv) (ITC_Type ty ITC_Nil))
#endif
                                | (tv,ty) <- ifConEqSpec decl]

extractFields :: DynFlags -> IfaceConDecl -> [SimpleDescr]
extractFields dflags decl = map (\ (n, t) -> SimpleDescr n t Nothing Nothing True)
                                $ zip (map (extractFieldNames . nameOccName82 . flSelector) (ifConFields decl))
                                        (map (extractType dflags) (conArgIfaceTys decl))

extractType :: DynFlags -> IfaceType -> Maybe ByteString
extractType dflags it = Just ((BS.pack . filterExtras . showSDoc dflags . ppr) it)

extractFieldNames :: OccName -> Text
extractFieldNames occName' = T.pack . unpackFS $occNameFS occName'

extractClassOp :: DynFlags -> IfaceClassOp -> SimpleDescr
extractClassOp dflags (IfaceClassOp occName' ty _dm) = SimpleDescr (T.pack . unpackFS . occNameFS $ nameOccName82 occName')
                                                (Just (BS.pack $ showSDoc dflags (ppr ty)))
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
    -> IfaceClsInst
    -> [Descr]
extractInstances dflags pm ifaceInst  =
    let className   =   showSDoc dflags $ ppr $ ifInstCls ifaceInst
        dataNames   =   map (T.pack . showSDoc dflags . ppr) . catMaybes $ ifInstTys ifaceInst
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
        ids     =   map (T.pack . showSDoc dflags . ppr . fst) usg_entities'
    in Just (name, Set.fromList ids)
extractUsages _ UsageFile {} = Nothing
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



