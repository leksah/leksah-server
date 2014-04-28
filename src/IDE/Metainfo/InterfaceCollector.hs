{-# OPTIONS_GHC -XScopedTypeVariables -XFlexibleContexts#-}
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
import Module hiding (PackageId,ModuleName)
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
#if MIN_VERSION_ghc(7,3,0)
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
#if MIN_VERSION_ghc(6,12,1)
import PackageConfig (PackageConfig, mkPackageId)
#else
import PackageConfig
       (PackageConfig, mkPackageId)
#endif
import TcRnTypes
import Data.Char (isSpace)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.Package hiding (PackageId)
import Distribution.ModuleName
import Distribution.Text (simpleParse)
import qualified Data.ByteString.Char8 as BS
import IDE.Core.Serializable ()
import IDE.Core.CTypes
import Data.ByteString.Char8 (ByteString)
import TcRnMonad (initTcRnIf)
import IDE.Utils.GHCUtils
import Control.DeepSeq(deepseq)

#if !MIN_VERSION_ghc(7,6,0)
showSDoc :: DynFlags -> SDoc -> String
showSDoc _ = O.showSDoc
showSDocUnqual :: DynFlags -> SDoc -> String
showSDocUnqual _ = O.showSDocUnqual
#endif

collectPackageFromHI :: PackageConfig -> IO PackageDescr
collectPackageFromHI  packageConfig = inGhcIO [] [] $ \ dflags -> do
    session             <-  getSession
    exportedIfaceInfos  <-  getIFaceInfos (getThisPackage packageConfig)
                                            (IPI.exposedModules packageConfig) session
    hiddenIfaceInfos    <-  getIFaceInfos (getThisPackage packageConfig)
                                            (IPI.hiddenModules packageConfig) session
    let pd = extractInfo dflags exportedIfaceInfos hiddenIfaceInfos (getThisPackage packageConfig)
                                            [] -- TODO 6.12 (IPI.depends $ packageConfigToInstalledPackageInfo packageConfig))
    deepseq pd (return pd)


getIFaceInfos :: PackageIdentifier -> [Module.ModuleName] -> HscEnv -> Ghc [(ModIface, FilePath)]
getIFaceInfos pid modules _session = do
    let isBase          =   pkgName pid == (PackageName "base")
    let ifaces          =   mapM (\ mn -> findAndReadIface empty
                                          (if isBase
                                                then mkBaseModule_ mn
                                                else mkModule (mkPackageId pid) mn)
                                          False) modules
    hscEnv              <-  getSession
    let gblEnv          =   IfGblEnv { if_rec_types = Nothing }
    maybes              <-  Hs.liftIO $ initTcRnIf  'i' hscEnv gblEnv () ifaces
    let res             =   catMaybes (map handleErr maybes)
    return res
    where
        handleErr (M.Succeeded val)   =   Just val
        handleErr (M.Failed _mess)    =   Nothing

-------------------------------------------------------------------------

extractInfo :: DynFlags -> [(ModIface, FilePath)] -> [(ModIface, FilePath)] -> PackageIdentifier ->
                    [PackageIdentifier] -> PackageDescr
extractInfo dflags ifacesExp ifacesHid pid buildDepends =
    let allDescrs           =   concatMap (extractExportedDescrH dflags pid)
                                    (map fst (ifacesHid ++ ifacesExp))
        mods                =   map (extractExportedDescrR dflags pid allDescrs) (map fst ifacesExp)
    in PackageDescr {
        pdPackage           =   pid
    ,   pdModules           =   mods
    ,   pdBuildDepends      =   buildDepends
    ,   pdMbSourcePath      =   Nothing}

extractExportedDescrH :: DynFlags -> PackageIdentifier -> ModIface -> [Descr]
extractExportedDescrH dflags pid iface =
    let mid                 =   (fromJust . simpleParse . moduleNameString . moduleName) (mi_module iface)
        exportedNames       =   Set.fromList
#if MIN_VERSION_Cabal(1,11,0)
                                $ map (occNameString . nameOccName)
                                    $ concatMap availNames
                                        $ mi_exports iface
#else
                                $ map occNameString
                                    $ concatMap availNames
                                        $ concatMap snd (mi_exports iface)
#endif
        exportedDecls       =   filter (\ ifdecl -> (occNameString $ ifName ifdecl)
                                                    `Set.member` exportedNames)
                                                            (map snd (mi_decls iface))
    in  concatMap (extractIdentifierDescr dflags pid [mid]) exportedDecls


extractExportedDescrR :: DynFlags
    -> PackageIdentifier
    -> [Descr]
    -> ModIface
    -> ModuleDescr
extractExportedDescrR dflags pid hidden iface =
    let mid             =   (fromJust . simpleParse . moduleNameString . moduleName) (mi_module iface)
        exportedNames   =   Set.fromList
#if MIN_VERSION_Cabal(1,11,0)
                                $ map (occNameString . nameOccName)
                                    $ concatMap availNames
                                        $ mi_exports iface
#else
                                $ map occNameString
                                    $ concatMap availNames
                                        $ concatMap snd (mi_exports iface)
#endif
        exportedDecls   =   filter (\ ifdecl -> (occNameString $ifName ifdecl)
                                                    `Set.member` exportedNames)
                                                            (map snd (mi_decls iface))
        ownDecls        =   concatMap (extractIdentifierDescr dflags pid [mid]) exportedDecls
        otherDecls      =   exportedNames `Set.difference` (Set.fromList (map dscName ownDecls))
        reexported      =   map (\d -> Reexported (ReexportedDescr (Just (PM pid mid)) d))
                                 $ filter (\k -> (dscName k) `Set.member` otherDecls) hidden
        inst            =   concatMap (extractInstances dflags (PM pid mid)) (mi_insts iface)
        uses            =   Map.fromList . catMaybes $ map (extractUsages dflags) (mi_usages iface)
        declsWithExp    =   map withExp ownDecls
        withExp (Real d) =  Real $ d{dscExported' = Set.member (dscName' d) exportedNames}
        withExp _        =  error "Unexpected Reexported"
    in  ModuleDescr {
                    mdModuleId          =   PM pid mid
                ,   mdMbSourcePath      =   Nothing
                ,   mdReferences        =   uses
                ,   mdIdDescriptions    =   declsWithExp ++ inst ++ reexported}

extractIdentifierDescr :: DynFlags -> PackageIdentifier -> [ModuleName] -> IfaceDecl -> [Descr]
extractIdentifierDescr dflags package modules decl
   = if null modules
      then []
      else
        let descr = RealDescr{
                    dscName'           =   unpackFS $occNameFS (ifName decl)
                ,   dscMbTypeStr'      =   Just (BS.pack $ unlines $ nonEmptyLines $ filterExtras $ showSDocUnqual dflags $ppr decl)
                ,   dscMbModu'         =   Just (PM package (last modules))
                ,   dscMbLocation'     =   Nothing
                ,   dscMbComment'      =   Nothing
                ,   dscTypeHint'       =   VariableDescr
                ,   dscExported'       =   True
                }
        in case decl of
            (IfaceId {}) -> map Real [descr]
            (IfaceData {ifName=name, ifCons=ifCons'})
                -> let d = case ifCons' of
                            IfDataTyCon _decls
                                ->  let
                                        fieldNames          =   concatMap (extractFields dflags) (visibleIfConDecls ifCons')
                                        constructors'       =   extractConstructors dflags name (visibleIfConDecls ifCons')
                                    in DataDescr constructors' fieldNames
                            IfNewTyCon _
                                ->  let
                                        fieldNames          =   concatMap (extractFields dflags) (visibleIfConDecls ifCons')
                                        constructors'       =   extractConstructors dflags name (visibleIfConDecls ifCons')
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
                            IfAbstractTyCon _ ->  DataDescr [] []
#else
                            IfAbstractTyCon ->  DataDescr [] []
#endif
#if MIN_VERSION_ghc(7,6,0)
                            IfDataFamTyCon ->  DataDescr [] []
#else
                            IfOpenDataTyCon ->  DataDescr [] []
#endif
                    in [Real (descr{dscTypeHint' = d})]
#if MIN_VERSION_ghc(7,6,0)
            (IfaceClass {ifCtxt = context, ifSigs = ifSigs'} )
#else
            (IfaceClass context _ _ _ _ ifSigs' _ )
#endif
                        ->  let
                                classOpsID          =   map (extractClassOp dflags) ifSigs'
                                superclasses        =   extractSuperClassNames context
                            in [Real $ descr{dscTypeHint' = ClassDescr superclasses classOpsID}]
            (IfaceSyn {})
                        ->  [Real $ descr{dscTypeHint' = TypeDescr}]
#if MIN_VERSION_ghc(7,6,0)
            (IfaceAxiom {})
                        ->  [Real $ descr]
#endif
            (IfaceForeign {})
                        ->  [Real $ descr]
#if MIN_VERSION_ghc(7,8,0)
            (IfacePatSyn {})
                        ->  [Real $ descr]
#endif

extractConstructors :: DynFlags -> OccName -> [IfaceConDecl] -> [SimpleDescr]
extractConstructors dflags name decls = map (\decl -> SimpleDescr (unpackFS $occNameFS (ifConOcc decl))
                                                 (Just (BS.pack $ filterExtras $ showSDocUnqual dflags $
                                                    pprIfaceForAllPart (ifConUnivTvs decl ++ ifConExTvs decl)
                                                        (eq_ctxt decl ++ ifConCtxt decl) (pp_tau decl)))
                                                 Nothing Nothing True) decls

    where
    pp_tau decl     = case map pprParendIfaceType (ifConArgTys decl) ++ [pp_res_ty decl] of
                    		(t:ts) -> fsep (t : map (arrow <+>) ts)
                    		[]     -> panic "pp_con_taus"
    pp_res_ty decl  = ppr name <+> fsep [ppr tv | (tv,_) <- ifConUnivTvs decl]
#if MIN_VERSION_ghc(7,3,0)
    eq_ctxt decl    = [IfaceTyConApp (IfaceTc eqTyConName) [(IfaceTyVar (occNameFS tv)), ty]
#else
    eq_ctxt decl    = [(IfaceEqPred (IfaceTyVar (occNameFS tv)) ty)
#endif
	                        | (tv,ty) <- ifConEqSpec decl]

extractFields :: DynFlags -> IfaceConDecl -> [SimpleDescr]
extractFields dflags decl = map (\ (n, t) -> SimpleDescr n t Nothing Nothing True)
                                $ zip (map extractFieldNames (ifConFields decl))
                                        (map (extractType dflags) (ifConArgTys decl))

extractType :: DynFlags -> IfaceType -> Maybe ByteString
extractType dflags it = Just ((BS.pack . filterExtras . showSDocUnqual dflags . ppr) it)

extractFieldNames :: OccName -> String
extractFieldNames occName' = unpackFS $occNameFS occName'

extractClassOp :: DynFlags -> IfaceClassOp -> SimpleDescr
extractClassOp dflags (IfaceClassOp occName' _dm ty) = SimpleDescr (unpackFS $occNameFS occName')
                                                (Just (BS.pack $ showSDocUnqual dflags (ppr ty)))
                                                Nothing Nothing True

extractSuperClassNames :: [IfacePredType] -> [String]
extractSuperClassNames l = catMaybes $ map extractSuperClassName l
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
        dataNames   =   map (\iftc -> showSDocUnqual dflags $ ppr iftc)
                            $ map fromJust
                                $ filter isJust
                                    $ ifInstTys ifaceInst
    in [Real (RealDescr
                    {   dscName'         =   className
                    ,   dscMbTypeStr'    =   Nothing
                    ,   dscMbModu'       =   Just pm
                    ,   dscMbLocation'   =   Nothing
                    ,   dscMbComment'    =   Nothing
                    ,   dscTypeHint'     =   InstanceDescr dataNames
                    ,   dscExported'     =   False})]


extractUsages :: DynFlags -> Usage -> Maybe (ModuleName, Set String)
extractUsages _ (UsagePackageModule {usg_mod = usg_mod'}) =
    let name    =   (fromJust . simpleParse . moduleNameString) (moduleName usg_mod')
    in Just (name, Set.fromList [])
extractUsages dflags (UsageHomeModule {usg_mod_name = usg_mod_name', usg_entities = usg_entities'}) =
    let name    =   (fromJust . simpleParse . moduleNameString) usg_mod_name'
        ids     =   map (showSDocUnqual dflags . ppr . fst) usg_entities'
    in Just (name, Set.fromList ids)
#if MIN_VERSION_ghc(7,4,0)
extractUsages _ (UsageFile _ _) = Nothing
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



