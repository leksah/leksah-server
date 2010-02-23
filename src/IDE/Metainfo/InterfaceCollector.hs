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
import HscTypes hiding (liftIO)
import qualified HscTypes as Hs (liftIO)
import LoadIface
import Outputable hiding(trace)
import IfaceSyn
import FastString
import Name
import PrelNames
#if MIN_VERSION_ghc(6,12,1)
import PackageConfig
       (PackageConfig(..), mkPackageId, packageConfigToInstalledPackageInfo)
#else
import PackageConfig
       (PackageConfig(..), mkPackageId)
#endif
import qualified PackageConfig as DP
import TcRnTypes
import qualified FastString as FS
import Data.Char (isSpace)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.Package hiding (PackageId)
import Distribution.ModuleName
import Distribution.Text (display, simpleParse)
import Control.Monad.Reader
import Data.Maybe
import System.FilePath
import qualified Data.ByteString.Char8 as BS
import IDE.Core.Serializable ()
import IDE.Core.CTypes
import Data.ByteString.Char8 (ByteString(..))
import TcRnMonad (initTcRnIf)
import Debug.Trace (trace)
import IDE.Utils.GHCUtils
import Control.DeepSeq(deepseq)

#if MIN_VERSION_Cabal(1,8,0)
getThisPackage    =   IPI.sourcePackageId
#else
getThisPackage    =   IPI.package
#endif

collectPackageFromHI :: PackageConfig -> IO PackageDescr
collectPackageFromHI  packageConfig = trace ("collectPackageFromHI " ++ display (getThisPackage packageConfig))
    $ inGhcIO [] [] $ \ _ -> do
    session             <-  getSession
    exportedIfaceInfos  <-  getIFaceInfos (getThisPackage packageConfig)
                                            (IPI.exposedModules packageConfig) session
    hiddenIfaceInfos    <-  getIFaceInfos (getThisPackage packageConfig)
                                            (IPI.hiddenModules packageConfig) session
    let pd = extractInfo exportedIfaceInfos hiddenIfaceInfos (getThisPackage packageConfig)
#if MIN_VERSION_Cabal(1,8,0)
                                            [] -- TODO 6.12 (IPI.depends $ packageConfigToInstalledPackageInfo packageConfig))
#else
				            (depends packageConfig)
#endif
    deepseq pd (return pd)


getIFaceInfos :: PackageIdentifier -> [Module.ModuleName] -> HscEnv -> Ghc [(ModIface, FilePath)]
getIFaceInfos pid modules session = do
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
        handleErr (M.Failed mess)     =   Nothing

-------------------------------------------------------------------------

extractInfo :: [(ModIface, FilePath)] -> [(ModIface, FilePath)] -> PackageIdentifier ->
                    [PackageIdentifier] -> PackageDescr
extractInfo  ifacesExp ifacesHid pi depends =
    let allDescrs           =   concatMap (extractExportedDescrH pi)
                                    (map fst (ifacesHid ++ ifacesExp))
        mods                =   map (extractExportedDescrR pi allDescrs) (map fst ifacesExp)
    in PackageDescr {
        pdPackage           =   pi
    ,   pdModules           =   mods
    ,   pdBuildDepends      =   depends
    ,   pdMbSourcePath      =   Nothing}

extractExportedDescrH :: PackageIdentifier -> ModIface -> [Descr]
extractExportedDescrH pid iface =
    let mid                 =   (fromJust . simpleParse . moduleNameString . moduleName) (mi_module iface)
        exportedNames       =   Set.fromList
                                $ map occNameString
                                    $ concatMap availNames
                                        $ concatMap snd (mi_exports iface)
        exportedDecls       =   filter (\ ifdecl -> (occNameString $ ifName ifdecl)
                                                    `Set.member` exportedNames)
                                                            (map snd (mi_decls iface))
    in  concatMap (extractIdentifierDescr pid [mid]) exportedDecls


extractExportedDescrR :: PackageIdentifier
    -> [Descr]
    -> ModIface
    -> ModuleDescr
extractExportedDescrR pid hidden iface =
    let mid             =   (fromJust . simpleParse . moduleNameString . moduleName) (mi_module iface)
        exportedNames   =   Set.fromList
                                $map occNameString
                                    $concatMap availNames
                                        $concatMap snd (mi_exports iface)
        exportedDecls   =   filter (\ ifdecl -> (occNameString $ifName ifdecl)
                                                    `Set.member` exportedNames)
                                                            (map snd (mi_decls iface))
        ownDecls        =   concatMap (extractIdentifierDescr pid [mid]) exportedDecls
        otherDecls      =   exportedNames `Set.difference` (Set.fromList (map dscName ownDecls))
        reexported      =   map (\d -> Reexported (ReexportedDescr (Just (PM pid mid)) d))
                                 $ filter (\k -> (dscName k) `Set.member` otherDecls) hidden
        inst            =   concatMap (extractInstances (PM pid mid)) (mi_insts iface)
        uses            =   Map.fromList $ map extractUsages (mi_usages iface)
        declsWithExp    =   map (\ (Real decl) -> Real $ decl{dscExported' =
                                                Set.member (dscName' decl) exportedNames})  ownDecls
    in  ModuleDescr {
                    mdModuleId          =   PM pid mid
                ,   mdMbSourcePath      =   Nothing
                ,   mdReferences        =   uses
                ,   mdIdDescriptions    =   declsWithExp ++ inst ++ reexported}

extractIdentifierDescr :: PackageIdentifier -> [ModuleName] -> IfaceDecl -> [Descr]
extractIdentifierDescr package modules decl
   = if null modules
      then []
      else
        let descr = RealDescr{
                    dscName'           =   unpackFS $occNameFS (ifName decl)
                ,   dscMbTypeStr'      =   Just (BS.pack $ unlines $ nonEmptyLines $ filterExtras $ showSDocUnqual $ppr decl)
                ,   dscMbModu'         =   Just (PM package (last modules))
                ,   dscMbLocation'     =   Nothing
                ,   dscMbComment'      =   Nothing
                ,   dscTypeHint'       =   VariableDescr
                ,   dscExported'       =   True
                }
        in case decl of
#if MIN_VERSION_Cabal(1,8,0)
            (IfaceId _ _ _ _)
#else
            (IfaceId _ _ _)
#endif
                -> map Real [descr]
            (IfaceData name _ _ ifCons _ _ _ _)
                -> let d = case ifCons of
                            IfDataTyCon decls
                                ->  let
                                        fieldNames          =   concatMap extractFields (visibleIfConDecls ifCons)
                                        constructors'       =   extractConstructors name (visibleIfConDecls ifCons)
                                    in DataDescr constructors' fieldNames
                            IfNewTyCon _
                                ->  let
                                        fieldNames          =   concatMap extractFields (visibleIfConDecls ifCons)
                                        constructors'       =   extractConstructors name (visibleIfConDecls ifCons)
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
                            IfAbstractTyCon ->  DataDescr [] []
                            IfOpenDataTyCon ->  DataDescr [] []
                    in [Real (descr{dscTypeHint' = d})]
            (IfaceClass context _ _ _ _ ifSigs _ )
                        ->  let
                                classOpsID          =   map extractClassOp ifSigs
                                superclasses        =   extractSuperClassNames context
                            in [Real $ descr{dscTypeHint' = ClassDescr superclasses classOpsID}]
            (IfaceSyn _ _ _ _ _ )
                        ->  [Real $ descr{dscTypeHint' = TypeDescr}]
            (IfaceForeign _ _)
                        ->  [Real $ descr]

extractConstructors ::   OccName -> [IfaceConDecl] -> [SimpleDescr]
extractConstructors name decls    =   map (\decl -> SimpleDescr (unpackFS $occNameFS (ifConOcc decl))
                                                 (Just (BS.pack $ filterExtras $ showSDocUnqual $
                                                    pprIfaceForAllPart (ifConUnivTvs decl ++ ifConExTvs decl)
                                                        (eq_ctxt decl ++ ifConCtxt decl) (pp_tau decl)))
                                                 Nothing Nothing True) decls

    where
    pp_tau decl     = case map pprParendIfaceType (ifConArgTys decl) ++ [pp_res_ty decl] of
                    		(t:ts) -> fsep (t : map (arrow <+>) ts)
                    		[]     -> panic "pp_con_taus"
    pp_res_ty decl  = ppr name <+> fsep [ppr tv | (tv,_) <- ifConUnivTvs decl]
    eq_ctxt decl    = [(IfaceEqPred (IfaceTyVar (occNameFS tv)) ty)
	                        | (tv,ty) <- ifConEqSpec decl]

extractFields ::  IfaceConDecl -> [SimpleDescr]
extractFields  decl    =   map (\ (n, t) -> SimpleDescr n t Nothing Nothing True)
                                $ zip (map extractFieldNames (ifConFields decl))
                                        (map extractType (ifConArgTys decl))

extractType :: IfaceType -> Maybe ByteString
extractType it = Just ((BS.pack . filterExtras . showSDocUnqual . ppr) it)

extractFieldNames :: OccName -> String
extractFieldNames occName = unpackFS $occNameFS occName

extractClassOp :: IfaceClassOp -> SimpleDescr
extractClassOp (IfaceClassOp occName dm ty) = SimpleDescr (unpackFS $occNameFS occName)
                                                (Just (BS.pack $ showSDocUnqual (ppr ty)))
                                                Nothing Nothing True

extractSuperClassNames :: [IfacePredType] -> [String]
extractSuperClassNames l = catMaybes $ map extractSuperClassName l
    where   extractSuperClassName (IfaceClassP name _)  =
                Just (unpackFS $occNameFS $ nameOccName name)
            extractSuperClassName _                     =   Nothing

extractInstances :: PackModule -> IfaceInst -> [Descr]
extractInstances pm ifaceInst  =
    let className   =   showSDocUnqual $ ppr $ ifInstCls ifaceInst
        dataNames   =   map (\iftc -> showSDocUnqual $ ppr iftc)
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


extractUsages :: Usage -> (ModuleName, Set String)
extractUsages (UsagePackageModule usg_mod _ ) =
    let name    =   (fromJust . simpleParse . moduleNameString) (moduleName usg_mod)
    in (name, Set.fromList [])
extractUsages (UsageHomeModule usg_mod_name _ usg_entities _) =
    let name    =   (fromJust . simpleParse . moduleNameString) usg_mod_name
        ids     =   map (showSDocUnqual . ppr . fst) usg_entities
    in (name, Set.fromList ids)

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



