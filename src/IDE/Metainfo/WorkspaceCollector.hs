{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables, PatternGuards, OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Metainfo.WorkspaceCollector
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

module IDE.Metainfo.WorkspaceCollector (

    collectWorkspace

,   sortByLoc
,   attachComments
,   uncommentData
,   uncommentDecl
,   printHsDoc
,   toComment
,   srcSpanToLocation
,   sigToByteString

) where


import Prelude ()
import Prelude.Compat
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import IDE.Utils.Utils
import IDE.Utils.GHCUtils
import GHC hiding(Id,Failed,Succeeded,ModuleName)
import Outputable hiding(trace, (<>))
import ErrUtils
import qualified Data.Map as Map
import Data.Map(Map)
import System.Directory
import Distribution.Package hiding (PackageId)
import Distribution.ModuleName
import System.FilePath
import qualified Data.ByteString.Char8 as BS
import Data.Binary.Shared
import IDE.Utils.FileUtils
import IDE.Core.Serializable ()
import IDE.Core.CTypes hiding (SrcSpan(..))
import Data.ByteString.Char8 (ByteString)
import DriverPipeline (preprocess)
import StringBuffer(hGetStringBuffer)
import Data.List(partition,sortBy,nub,find)
import Data.Ord(comparing)
import GHC.Exception
import LoadIface(findAndReadIface)
import Distribution.Text(display)
import TcRnMonad (initTcRnIf, IfGblEnv(..))
import qualified Maybes as M
import IDE.Metainfo.InterfaceCollector
import Data.Maybe
       (isJust, catMaybes, isNothing, mapMaybe, listToMaybe)
import PrelNames
import System.Log.Logger
import Control.DeepSeq (deepseq)
#if MIN_VERSION_ghc(8,6,0)
import FastString (unpackFS)
#else
import FastString (mkFastString,appendFS,nullFS,unpackFS)
#endif
import Control.Monad.IO.Class (MonadIO, MonadIO(..))
import Control.Monad (when)
import Control.Exception as E
import Data.Text (Text)
import qualified Data.Text as T
#if MIN_VERSION_ghc(8,2,0)
import DynFlags (thisPackage)
import Module (toInstalledUnitId, InstalledModule(..), UnitId(..), DefUnitId(..))
#if MIN_VERSION_ghc(8,10,2)
import GHC.Hs.ImpExp (ieWrappedName)
#else
import HsImpExp (ieWrappedName)
#endif
#endif
#if !MIN_VERSION_ghc(8,4,0)
import Data.Kind (Constraint)
#endif
import IDE.Utils.Project (ProjectKey)
import Data.Foldable (forM_)
import Bag (bagToList)


#if !MIN_VERSION_ghc(8,2,0)
ieWrappedName :: RdrName -> RdrName
ieWrappedName = id
type OutputableBndrId = OutputableBndr
toInstalledUnitId :: GHC.UnitId -> GHC.UnitId
toInstalledUnitId = id
#endif

#if !MIN_VERSION_ghc(8,4,0)
type GhcPs = RdrName
type family IdP p
type instance IdP GhcPs = RdrName
type SourceTextX a = (() :: Constraint)
#endif

type NDecl = LHsDecl GhcPs
type NSig  = Located (Sig GhcPs)
type NModule = HsModule GhcPs
type NIE = IE GhcPs
type NLIE = LIE GhcPs

myDocEmpty :: NDoc
myDocAppend :: NDoc -> NDoc -> NDoc
isEmptyDoc :: NDoc -> Bool

type NDoc  = HsDocString
type MyLDocDecl = LDocDecl

#if MIN_VERSION_ghc(8,6,0)
myDocEmpty=mkHsDocString ""
myDocAppend l r = mkHsDocString ((unpackHDS l) <> (unpackHDS r))
isEmptyDoc fs = null (unpackHDS fs)
#else
myDocEmpty=HsDocString(mkFastString "")
myDocAppend (HsDocString l) (HsDocString r) = HsDocString (appendFS l r)
isEmptyDoc (HsDocString fs) = nullFS fs
#endif

showRdrName :: DynFlags -> RdrName -> Text
showRdrName dflags r = T.pack . showSDoc dflags $ ppr r

collectWorkspace :: PackageIdentifier -> [(Text,FilePath)] -> Bool -> Bool -> ProjectKey -> FilePath -> IO()
collectWorkspace pid moduleList forceRebuild writeAscii project package = do
    debugM "leksah-server" $ "collectWorkspace called with modules " ++ show moduleList ++ " in project " ++ show project ++ " package " ++ package
    collectorPath <- liftIO getCollectorPath
    let packageCollectorPath = collectorPath </> T.unpack (packageIdentifierToString pid)
    when forceRebuild $ do
        exists <- doesDirectoryExist packageCollectorPath
        when exists $ removeDirectoryRecursive packageCollectorPath
    -- Construct directory
    liftIO $ createDirectoryIfMissing True packageCollectorPath
    setCurrentDirectory (dropFileName package)
    opts1 <- filterOpts <$> figureOutGhcOpts (Just project) package
    opts2 <- figureOutHaddockOpts (Just project) package

    debugM "leksah-server" $ "before collect modules" ++ "\n\nopts1: " ++ show opts1 ++ "\n\n opt2: " ++ show opts2
    getSysLibDir Nothing (Just VERSION_ghc) >>= \case
        Nothing -> debugM "leksah-server" $ "collectWorkspace could not find system lib dir for GHC " <> VERSION_ghc <> " (used to build Leksah)"
        Just libDir -> mapM_ (collectModule libDir packageCollectorPath writeAscii pid opts1) moduleList
    debugM "leksah-server" "after collect modules"
  where
    filterOpts :: [Text] -> [Text]
    filterOpts []    = []
    filterOpts (o:_:r) | o `elem` ["-link-js-lib", "-js-lib-outputdir", "-js-lib-src", "-package-id"] = filterOpts r
    filterOpts (o:r) = o:filterOpts r

collectModule :: FilePath -> FilePath -> Bool -> PackageIdentifier -> [Text] -> (Text,FilePath) -> IO()
collectModule libDir collectorPackagePath writeAscii pid opts (modId,sourcePath) =
    case parseModuleKey (T.unpack modId) sourcePath of
        Nothing -> errorM "leksah-server" (T.unpack $ "Can't parse module name " <> modId)
        Just modKey -> do
            let collectorModulePath = collectorPackagePath </> moduleCollectorFileName modKey <.> leksahMetadataWorkspaceFileExtension
                moduleName' = moduleKeyToName modKey
            existCollectorFile <- doesFileExist collectorModulePath
            existSourceFile    <- doesFileExist sourcePath
            if existSourceFile
                then
                    if not existCollectorFile
                        then collectModule' libDir sourcePath collectorModulePath writeAscii pid opts moduleName'
                        else do
                            sourceModTime <-  getModificationTime sourcePath
                            collModTime   <-  getModificationTime collectorModulePath
                            when (sourceModTime > collModTime) $
                                collectModule' libDir sourcePath collectorModulePath writeAscii pid
                                        opts moduleName'
                else errorM "leksah-server" ("source file not found " ++ sourcePath)


collectModule' :: FilePath -> FilePath -> FilePath -> Bool -> PackageIdentifier -> [Text] -> ModuleName -> IO()
collectModule' libDir sourcePath destPath writeAscii pid opts moduleName' = gcatch (
  inGhcIO libDir (opts++["-cpp"]) [Opt_Haddock] [] $ \ dynFlags -> do
    session         <-  getSession
#if MIN_VERSION_ghc(8,8,0)
    liftIO (preprocess session sourcePath Nothing Nothing) >>= \case
      Left errMsg ->
        liftIO . errorM "leksah-server" $ "Failed to preprocess " <> sourcePath
      Right (dynFlags3,fp') -> do
#else
    liftIO (preprocess session (sourcePath,Nothing)) >>= \(dynFlags3,fp') -> do
#endif
        let packIdAndKey = PackageIdAndKey pid
                                (toInstalledUnitId $ thisPackage dynFlags3)
        mbInterfaceDescr <- mayGetInterfaceDescription dynFlags packIdAndKey moduleName'
        liftIO $ do
            stringBuffer    <-  hGetStringBuffer fp'
            parseResult     <-  myParseModule dynFlags3 sourcePath (Just stringBuffer)
            case parseResult of
                Right (L _ hsMod@HsModule{}) -> do
                    let moduleDescr = extractModDescr dynFlags pid moduleName' sourcePath hsMod
                    let moduleDescr' = case mbInterfaceDescr of
                                            Nothing -> moduleDescr
                                            Just md  -> mergeWithInterfaceDescr moduleDescr md
                    E.catch (writeExtractedModule destPath writeAscii moduleDescr')
                        (\ (_:: IOException) -> errorM "leksah-server" ("Can't write extracted package " ++ destPath))
                Left errMsgs -> do
                  let errorString = showSDoc dynFlags3 $ vcat $ pprErrMsgBagWithLoc errMsgs
                  errorM "leksah-server" $ "Failed to parse " ++ sourcePath ++ " " ++ errorString
                  forM_ (listToMaybe $ bagToList errMsgs) $ \errMsg -> do
                    let moduleDescr =  ModuleDescr {
                        mdModuleId          =   PM pid moduleName'
                    ,   mdMbSourcePath      =   Just sourcePath
                    ,   mdReferences        =   Map.empty -- imports
                    ,   mdIdDescriptions    =   [Real RealDescr {
                            dscName'        =   "Parse Error"
                        ,   dscMbTypeStr'   =   Nothing
                        ,   dscMbModu'      =   Just (PM pid moduleName')
                        ,   dscMbLocation'  =   srcSpanToLocation $ errMsgSpan errMsg
                        ,   dscMbComment'   =   Just (BS.pack $ errorString)
                        ,   dscTypeHint'    =   ErrorDescr
                        ,   dscExported'    =   False}]}
                    E.catch (deepseq moduleDescr $ writeExtractedModule destPath writeAscii moduleDescr)
                        (\ (_:: IOException) -> errorM "leksah-server" ("Can't write extracted module " ++ destPath))
    ) (\ (e :: SomeException) -> errorM "leksah-server" ("Can't extract module " ++ destPath ++ " " ++ show e))


writeExtractedModule :: MonadIO m => FilePath -> Bool -> ModuleDescr -> m ()
writeExtractedModule filePath writeAscii md =
    if writeAscii
        then liftIO $ writeFile (filePath ++ "dpg") (show md)
        else liftIO $ encodeFileSer filePath (metadataVersion, md)

-----------------------------------------------------------------------------------
-- Format conversion
extractModDescr
  :: DynFlags
  -> PackageIdentifier
  -> ModuleName
  -> FilePath
  -> NModule
  -> ModuleDescr
extractModDescr dflags pid moduleName' sourcePath hsMod = ModuleDescr {
        mdModuleId          =   PM pid moduleName'
    ,   mdMbSourcePath      =   modFile $ hsmodName hsMod
    ,   mdReferences        =   Map.empty -- imports
    ,   mdIdDescriptions    =   descrs'}
    where
        descrs = extractDescrs dflags (PM pid moduleName') (hsmodDecls hsMod)
        descrs' = fixExports dflags (unLoc <$> hsmodExports hsMod) descrs
        modFile (Just (L loc _)) =
            (locationFile <$> srcSpanToLocation loc) <|> Just sourcePath
        modFile _ = Just sourcePath

-----------------------------------------------------------------------------------
-- Add exported hint

fixExports
  :: DynFlags
  -> Maybe [NLIE]
  -> [Descr]
  -> [Descr]
fixExports _ Nothing descrs = descrs
fixExports dflags (Just iel) descrs = map (fixDescr (map unLoc iel)) descrs
    where
        fixDescr
          :: [NIE]
          -> Descr
          -> Descr
        fixDescr _ d@(Reexported _) = d
        fixDescr list (Real rd) = Real rd'
            where
                rd' = case dscTypeHint' rd of
                          VariableDescr       -> rd{dscExported' = isJust findVar}
                          PatternSynonymDescr -> rd{dscExported' = isJust findVar}
                          InstanceDescr _     -> rd
                          _                   -> case findThing of
                                                    Nothing                -> nothingExported rd
                                                    Just (IEThingAll {})   -> allExported rd
                                                    Just (IEThingAbs {})   -> someExported rd []
#if MIN_VERSION_ghc(8,6,0)
                                                    Just (IEThingWith _ _ _ l _) -> someExported rd (map (showRdrName dflags . ieWrappedName . unLoc) l)
#elif MIN_VERSION_ghc(8,0,0)
                                                    Just (IEThingWith _ _ l _) -> someExported rd (map (showRdrName dflags . ieWrappedName . unLoc) l)
#else
                                                    Just (IEThingWith _ l) -> someExported rd (map (showRdrName dflags . unLoc) l)
#endif
                                                    _                      -> allExported rd
                findVar = find (\ a ->
                            case a of
#if MIN_VERSION_ghc(8,6,0)
                                IEVar _ r | showRdrName dflags (ieWrappedName $ unLoc r) == dscName' rd -> True
#else
                                IEVar r | showRdrName dflags (ieWrappedName $ unLoc r) == dscName' rd -> True
#endif
                                _                                     -> False)
                                    list
                findThing = find (\ a ->
                                case a of
#if MIN_VERSION_ghc(8,6,0)
                                IEThingAbs _ r | showRdrName dflags (ieWrappedName $ unLoc r) == dscName' rd -> True
                                IEThingAll _ r | showRdrName dflags (ieWrappedName $ unLoc r) == dscName' rd -> True
                                IEThingWith _ r _ _list _ | showRdrName dflags (ieWrappedName $ unLoc r) == dscName' rd -> True
#else
                                IEThingAbs r | showRdrName dflags (ieWrappedName $ unLoc r) == dscName' rd -> True
                                IEThingAll r | showRdrName dflags (ieWrappedName $ unLoc r) == dscName' rd -> True
#if MIN_VERSION_ghc(8,0,0)
                                IEThingWith r _ _list _ | showRdrName dflags (ieWrappedName $ unLoc r) == dscName' rd -> True
#else
                                IEThingWith r _list | showRdrName dflags (unLoc r) == dscName' rd -> True
#endif
#endif
                                _                                     -> False)
                                    list
        allExported rd                                 = rd
        nothingExported rd                             = rd{dscExported' = False,
                                                             dscTypeHint' = nothingExportedS (dscTypeHint' rd)}
        nothingExportedS (DataDescr lsd1 lsd2)         = DataDescr (map (setExportedSD False) lsd1)
                                                            (map (setExportedSD False) lsd2)
        nothingExportedS (NewtypeDescr sd1 Nothing)    = NewtypeDescr (setExportedSD False sd1)
                                                            Nothing
        nothingExportedS (NewtypeDescr sd1 (Just _sd2)) = NewtypeDescr (setExportedSD False sd1)
                                                            (Just (setExportedSD False sd1))
        nothingExportedS (ClassDescr n lsd2)           = ClassDescr n (map (setExportedSD False) lsd2)
        nothingExportedS other                         = other

        someExported rd l                              = rd{dscExported' = True,
                                                            dscTypeHint' = someExportedS (dscTypeHint' rd) l}
        someExportedS (DataDescr lsd1 lsd2) l          = DataDescr (map (maySetExportedSD l) lsd1)
                                                            (map (maySetExportedSD l) lsd2)
        someExportedS (NewtypeDescr sd1 Nothing) l     = NewtypeDescr (maySetExportedSD l sd1)
                                                            Nothing
        someExportedS (NewtypeDescr sd1 (Just _sd2)) l  = NewtypeDescr (maySetExportedSD l sd1)
                                                            (Just (maySetExportedSD l sd1))
        someExportedS (ClassDescr n lsd2) l            = ClassDescr n (map (maySetExportedSD l) lsd2)
        someExportedS other _                          = other


        setExportedSD bool sd = sd{sdExported = bool}
        maySetExportedSD list sd = sd{sdExported = sdName sd `elem` list}


extractDescrs
  :: DynFlags
  -> PackModule
  -> [NDecl]
  -> [Descr]
extractDescrs dflags pm decls = transformToDescrs dflags pm tripleWithSigs
    where
        sortedDecls                    = sortByLoc decls
        pairedWithDocs                 = collectDocs sortedDecls
        filteredDecls                  = filterUninteresting pairedWithDocs
        (withoutSignatures,signatures) = partitionSignatures filteredDecls
        tripleWithSigs                 = attachSignatures dflags signatures withoutSignatures

-- | Sort by source location
sortByLoc :: [Located a] -> [Located a]
sortByLoc = sortBy (comparing getLoc)

filterUninteresting
  :: [(NDecl, Maybe NDoc)]
  -> [(NDecl, Maybe NDoc)]
filterUninteresting = filter filterSignature
    where
    filterSignature (L _srcDecl (SpliceD {}),_)  = False
    filterSignature (L _srcDecl (RuleD {}),_)    = False
    filterSignature (L _srcDecl (WarningD {}),_) = False
    filterSignature (L _srcDecl (ForD {}),_)     = False
    filterSignature (L _srcDecl (DefD {}),_)     = False
    filterSignature _                            = True

partitionSignatures
  :: [(NDecl, Maybe NDoc)]
  -> ([(NDecl, Maybe NDoc)], [(NDecl, Maybe NDoc)])
partitionSignatures = partition filterSignature
    where
    filterSignature (L _srcDecl (SigD {}),_) = False
    filterSignature _ = True

--partitionInstances :: [(NDecl,Maybe NDoc)] -> ([(NDecl,Maybe NDoc)],[(NDecl,Maybe NDoc)])
--partitionInstances i = (i,[])
--partition filterInstances
--    where
--    filterInstances ((L srcDecl (InstD _)),_) = False
--    filterInstances _ = True

-- | Collect the docs and attach them to the right declaration.
collectDocs
  :: [LHsDecl GhcPs]
  -> [(LHsDecl GhcPs, Maybe NDoc)]
collectDocs = collect Nothing myDocEmpty

collect
  :: Maybe NDecl
  -> NDoc
  -> [NDecl]
  -> [(NDecl, Maybe NDoc)]
collect d doc_so_far [] =
   case d of
        Nothing -> []
        Just d0  -> finishedDoc d0 doc_so_far []

collect d doc_so_far (e:es) =
  case e of
#if MIN_VERSION_ghc(8,6,0)
    L _ (DocD _ (DocCommentNext str)) ->
#else
    L _ (DocD (DocCommentNext str)) ->
#endif
      case d of
        Nothing -> collect d (myDocAppend doc_so_far str) es
        Just d0 -> finishedDoc d0 doc_so_far (collect Nothing str es)

#if MIN_VERSION_ghc(8,6,0)
    L _ (DocD _ (DocCommentPrev str)) -> collect d (myDocAppend doc_so_far str) es
#else
    L _ (DocD (DocCommentPrev str)) -> collect d (myDocAppend doc_so_far str) es
#endif

    _ -> case d of
      Nothing -> collect (Just e) doc_so_far es
      Just d0 -> finishedDoc d0 doc_so_far (collect (Just e) myDocEmpty es)

finishedDoc
  :: NDecl
  -> NDoc
  -> [(NDecl, Maybe NDoc)]
  -> [(NDecl, Maybe NDoc)]
finishedDoc d doc rest | isEmptyDoc doc = (d, Nothing) : rest
finishedDoc d doc rest | notDocDecl d = (d, Just doc) : rest
  where
    notDocDecl (L _ (DocD {})) = False
    notDocDecl _               = True
finishedDoc _ _ rest = rest

#if MIN_VERSION_ghc(8,4,0)
sigNameNoLoc' :: Sig pass -> [IdP pass]
#else
sigNameNoLoc' :: Sig name -> [name]
#endif
#if MIN_VERSION_ghc(8,6,0)
sigNameNoLoc' (TypeSig   _ ns _)          = map unLoc ns
sigNameNoLoc' (SpecSig   _ n _ _)         = [unLoc n]
sigNameNoLoc' (InlineSig _ n _)           = [unLoc n]
sigNameNoLoc' (FixSig _ (FixitySig _ ns _)) = map unLoc ns
#else
#if MIN_VERSION_ghc(8,0,0)
sigNameNoLoc' (TypeSig   ns _)          = map unLoc ns
#else
sigNameNoLoc' (TypeSig   ns _ _)        = map unLoc ns
#endif
sigNameNoLoc' (SpecSig   n _ _)         = [unLoc n]
sigNameNoLoc' (InlineSig n _)           = [unLoc n]
sigNameNoLoc' (FixSig (FixitySig ns _)) = map unLoc ns
#endif
sigNameNoLoc' _                         = []

attachSignatures
  :: DynFlags
  -> [(NDecl, Maybe NDoc)]
  -> [(NDecl, Maybe NDoc)]
  -> [(NDecl, Maybe NDoc, [(NSig,Maybe NDoc)])]
attachSignatures dflags signatures = map (attachSignature signaturesMap)
    where
    signaturesMap = Map.fromListWith (++) $ concatMap sigMap signatures

    sigMap
      :: (NDecl, Maybe NDoc)
      -> [(IdP GhcPs, [(NSig,Maybe NDoc)])]
#if MIN_VERSION_ghc(8,6,0)
    sigMap (L loc (SigD _ sig),c) | nameList <- sigNameNoLoc' sig =
#else
    sigMap (L loc (SigD sig),c) | nameList <- sigNameNoLoc' sig =
#endif
        map (\n -> (n, [(L loc sig,c)])) nameList
    sigMap v = error ("Unexpected location type" ++ (showSDoc dflags . ppr) v)

    attachSignature
      :: Map (IdP GhcPs) [(NSig,Maybe NDoc)]
      -> (NDecl, Maybe NDoc)
      -> (NDecl, Maybe NDoc, [(NSig,Maybe NDoc)])
    attachSignature signaturesMap'  (decl,mbDoc) =
        case declName (unLoc decl) of
            Nothing -> (decl,mbDoc, [])
            Just name -> case name `Map.lookup` signaturesMap' of
                            Just sigList -> (decl,mbDoc, sigList)
                            Nothing ->  (decl, mbDoc, [])
#if MIN_VERSION_ghc(8,6,0)
    declName _t@(TyClD _ x)                         = Just (tcdName x)
    declName _t@(ValD _ FunBind {fun_id = fun_id'}) = Just (unLoc fun_id')
#else
    declName _t@(TyClD x)                           = Just (tcdName x)
    declName _t@(ValD FunBind {fun_id = fun_id'})   = Just (unLoc fun_id')
#endif
    declName _                                      = Nothing


transformToDescrs
  :: DynFlags
  -> PackModule
  -> [(NDecl, Maybe NDoc, [(NSig, Maybe NDoc)])] -> [Descr]
transformToDescrs dflags pm = concatMap transformToDescr
    where
    transformToDescr
      :: (NDecl, Maybe NDoc, [(NSig, Maybe NDoc)]) -> [Descr]
#if MIN_VERSION_ghc(8,6,0)
    transformToDescr (L loc (ValD _ FunBind {fun_id = lid}), mbComment, sigList) =
#else
    transformToDescr (L loc (ValD FunBind {fun_id = lid}), mbComment, sigList) =
#endif
        [Real RealDescr {
        dscName'        =   showRdrName dflags (unLoc lid)
    ,   dscMbTypeStr'   =   sigToByteString dflags sigList
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment (mapMaybe snd sigList)
    ,   dscTypeHint'    =   VariableDescr
    ,   dscExported'    =   True}]

#if MIN_VERSION_ghc(8,6,0)
    transformToDescr (L loc (ValD _ (PatSynBind _ PSB{..})), mbComment,sigList) =
#else
    transformToDescr (L loc (ValD (PatSynBind PSB{..})), mbComment,sigList) =
#endif
        [Real RealDescr {
        dscName'        =   showRdrName dflags (unLoc psb_id)
    ,   dscMbTypeStr'   =   sigToByteString dflags sigList
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment (catMaybes (map snd sigList))
    ,   dscTypeHint'    =   PatternSynonymDescr
    ,   dscExported'    =   True}]

#if MIN_VERSION_ghc(8,6,0)
    transformToDescr (L loc for@(ForD _ (ForeignImport {fd_name = lid})), mbComment,_sigList) =
#else
    transformToDescr (L loc for@(ForD (ForeignImport {fd_name = lid})), mbComment,_sigList) =
#endif
        [Real RealDescr {
        dscName'        =   showRdrName dflags (unLoc lid)
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr for))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]

#if MIN_VERSION_ghc(8,6,0)
    transformToDescr (L loc (TyClD _ typ@FamDecl {tcdFam = FamilyDecl{ fdLName = lid}}), mbComment,_sigList) =
#else
    transformToDescr (L loc (TyClD typ@FamDecl {tcdFam = FamilyDecl{ fdLName = lid}}), mbComment,_sigList) =
#endif
        [Real RealDescr {
        dscName'        =   showRdrName dflags (unLoc lid)
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr typ))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]

#if MIN_VERSION_ghc(8,6,0)
    transformToDescr (L loc (TyClD _ typ@SynDecl {tcdLName = lid}), mbComment,_sigList) =
#else
    transformToDescr (L loc (TyClD typ@SynDecl {tcdLName = lid}), mbComment,_sigList) =
#endif
        [Real RealDescr {
        dscName'        =   showRdrName dflags (unLoc lid)
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr typ))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]

#if MIN_VERSION_ghc(8,6,0)
    transformToDescr (L loc (TyClD _ typ@(DataDecl {tcdLName = lid, tcdDataDefn = HsDataDefn {dd_cons=lConDecl, dd_derivs=tcdDerivs'}})), mbComment,_sigList) =
#else
    transformToDescr (L loc (TyClD typ@(DataDecl {tcdLName = lid, tcdDataDefn = HsDataDefn {dd_cons=lConDecl, dd_derivs=tcdDerivs'}})), mbComment,_sigList) =
#endif
        Real RealDescr {
        dscName'        =   name
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr (uncommentData typ)))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   DataDescr constructors fields
    ,   dscExported'    =   True}
            : derivings tcdDerivs'
        where
        constructors    =   concatMap (extractConstructor dflags) lConDecl
        fields          =   nub $ concatMap (extractRecordFields dflags) lConDecl
        name            =   showRdrName dflags (unLoc lid)
#if MIN_VERSION_ghc(8,2,0)
        derivings
          :: HsDeriving GhcPs
          -> [Descr]
        derivings l = concatMap (extractDeriving dflags pm name) (unLoc l)
#else
        derivings Nothing = []
        derivings (Just l) = map (extractDeriving dflags pm name) (unLoc l)
#endif

#if MIN_VERSION_ghc(8,6,0)
    transformToDescr (L loc (TyClD _ cl@ClassDecl{tcdLName=tcdLName', tcdSigs=tcdSigs', tcdDocs=docs}), mbComment,_sigList) =
#else
    transformToDescr (L loc (TyClD cl@ClassDecl{tcdLName=tcdLName', tcdSigs=tcdSigs', tcdDocs=docs}), mbComment,_sigList) =
#endif
        [Real RealDescr {
        dscName'        =   showRdrName dflags (unLoc tcdLName')
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual dflags $ppr cl{tcdMeths = emptyLHsBinds}))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   ClassDescr super methods
    ,   dscExported'    =   True    }]
        where
        methods         =   extractMethods dflags tcdSigs' docs
        super           =   []

#if MIN_VERSION_ghc(8,6,0)
    transformToDescr (L loc (InstD _ inst), mbComment, _sigList) =
#else
    transformToDescr (L loc (InstD inst), mbComment, _sigList) =
#endif
        let typp = case inst of
#if MIN_VERSION_ghc(8,6,0)
                     ClsInstD _ t -> ppr t
                     DataFamInstD _ t -> ppr t
                     TyFamInstD _ t -> ppr t
                     XInstDecl t -> ppr t
#else
                     ClsInstD t -> ppr t
                     DataFamInstD t -> ppr t
                     TyFamInstD t -> ppr t
#endif
            (instn,nameI,other) =   case T.words . T.pack $ showSDocUnqual dflags typp of
                                        instn':nameI':tl -> (instn',nameI',takeWhile (/= "where") tl)
                                        _ -> ("","",[])
        in
            [Real $ RealDescr {
            dscName'        =   instn <> " " <> nameI
        ,   dscMbTypeStr'   =   Just (BS.pack . T.unpack $ instn <> " " <> nameI <> " " <> (T.intercalate " " other))
        ,   dscMbModu'      =   Just pm
        ,   dscMbLocation'  =   srcSpanToLocation loc
        ,   dscMbComment'   =   toComment mbComment []
        ,   dscTypeHint'    =   InstanceDescr other
        ,   dscExported'    =   True}]
            where

    transformToDescr (_, _mbComment, _sigList) = []


uncommentData :: TyClDecl a -> TyClDecl a
uncommentData td@(DataDecl {tcdDataDefn = def'@(HsDataDefn{dd_cons = conDecls})}) = td{
    tcdDataDefn = def'{dd_cons = map uncommentDecl conDecls}}
uncommentData other                            = other

uncommentDecl :: LConDecl a -> LConDecl a
#if MIN_VERSION_ghc(8,0,1)
#if MIN_VERSION_ghc(8,6,0)
uncommentDecl (L l cd@ConDeclGADT{}) =
    L l cd{con_args = uncommentDetails (con_args cd), con_doc = Nothing}
uncommentDecl (L l cd@ConDeclH98{}) =
    L l cd{con_args = uncommentDetails (con_args cd), con_doc = Nothing}
uncommentDecl (L l cd@XConDecl{}) =
    L l cd{con_args = uncommentDetails (con_args cd), con_doc = Nothing}
#else
uncommentDecl (L l cd@ConDeclGADT{}) =
    L l cd{con_details = uncommentDetails (con_details cd), con_doc = Nothing}
uncommentDecl (L l cd@ConDeclH98{}) =
    L l cd{con_details = uncommentDetails (con_details cd), con_doc = Nothing}
#endif
#else
uncommentDecl (L l cd) =
    L l cd{con_details= uncommentDetails (con_details cd)}
#endif

uncommentDetails :: HsConDeclDetails a -> HsConDeclDetails a
uncommentDetails (RecCon (L l flds)) = RecCon (L l (map uncommentField flds))
    where
    uncommentField (L l2 cdf)  =  L l2 (cdf {cd_fld_doc = Nothing})
uncommentDetails other = other

mergeWithInterfaceDescr :: ModuleDescr -> ModuleDescr -> ModuleDescr
mergeWithInterfaceDescr md imd = md {
    mdReferences = mdReferences imd,
    mdIdDescriptions = mergeIdDescrs (mdIdDescriptions md) (mdIdDescriptions imd)}

mergeIdDescrs :: [Descr] -> [Descr] -> [Descr]
mergeIdDescrs d1 d2 = dres ++ reexported
    where
        (reexported,real)  = partition isReexported d2
        lm = Map.fromList $ zip (map (dscName &&& dscTypeHint) real) real
        dres =  map (addType lm) d1

        addType lm' (Real d1') | isNothing (dscMbTypeStr' d1') =
            Real $ d1'{dscMbTypeStr' = case (dscName' d1', dscTypeHint' d1') `Map.lookup` lm' of
                                        Nothing -> Nothing
                                        Just d -> dscMbTypeStr d}
        addType _ d                     = d

#if MIN_VERSION_ghc(8,2,0)
#if MIN_VERSION_ghc(8,6,0)
extractDeriving :: (Outputable (HsType alpha)) => DynFlags -> PackModule -> Text -> LHsDerivingClause alpha -> [Descr]
extractDeriving _ _ _ (L _ XHsDerivingClause {}) = []
extractDeriving dflags pm name (L _ HsDerivingClause {deriv_clause_tys = L _ c}) = c >>= extractDeriving' dflags pm name
#else
extractDeriving :: (SourceTextX alpha, OutputableBndrId alpha) => DynFlags -> PackModule -> Text -> LHsDerivingClause alpha -> [Descr]
extractDeriving dflags pm name (L _ HsDerivingClause {deriv_clause_tys = L _ c}) = map (extractDeriving' dflags pm name) c
#endif

#if MIN_VERSION_ghc(8,6,0)
-- extractDeriving' :: (alpha ~ GhcPass pass, OutputableBndrId alpha) => DynFlags -> PackModule -> Text -> LHsSigType alpha -> [Descr]
extractDeriving' _ _ _ (XHsImplicitBndrs {}) = []
extractDeriving' dflags pm name (HsIB { hsib_body = (L loc typ) }) = [ descr ]
#else
extractDeriving' :: (SourceTextX alpha, OutputableBndrId alpha) => DynFlags -> PackModule -> Text -> LHsSigType alpha -> Descr
extractDeriving' dflags pm name (HsIB { hsib_body = (L loc typ) }) = descr
#endif
#elif MIN_VERSION_ghc(8,0,0)
extractDeriving :: OutputableBndr alpha => DynFlags -> PackModule -> Text -> LHsSigType alpha -> Descr
extractDeriving dflags pm name HsIB { hsib_body = (L loc typ) } = descr
#else
extractDeriving :: OutputableBndr alpha => DynFlags -> PackModule -> Text -> LHsType alpha -> Descr
extractDeriving dflags pm name (L loc typ) = descr
#endif
  where
    descr = Real RealDescr {
          dscName'        =   className
      ,   dscMbTypeStr'   =   Just (BS.pack . T.unpack $ "instance " <> className <> " " <> name)
      ,   dscMbModu'      =   Just pm
      ,   dscMbLocation'  =   srcSpanToLocation loc
      ,   dscMbComment'   =   toComment (Nothing :: Maybe NDoc) []
      ,   dscTypeHint'    =   InstanceDescr (T.words name)
      ,   dscExported'    =   True}
    className = T.pack . showSDocUnqual dflags $ ppr typ

extractMethods
  :: DynFlags
  -> [NSig]
  -> [MyLDocDecl]
  -> [SimpleDescr]
extractMethods dflags sigs docs =
    let pairs = attachComments sigs docs
    in concatMap (extractMethod dflags) pairs

#if MIN_VERSION_ghc(8,6,0)
-- extractMethod :: (alpha ~ GhcPass pass, OutputableBndrId alpha) => DynFlags -> (LHsDecl alpha, Maybe NDoc) -> [SimpleDescr]
extractMethod dflags (L loc (SigD _ ts@(TypeSig _ names _typ)), mbDoc) = map (extractMethodName dflags loc ts mbDoc) names
extractMethod dflags (L loc (SigD _ ts@(PatSynSig _ names _typ)), mbDoc) = map (extractMethodName dflags loc ts mbDoc) names
extractMethod dflags (L loc (SigD _ ts@(ClassOpSig _ _ names _typ)), mbDoc) = map (extractMethodName dflags loc ts mbDoc) names
#else
extractMethod :: (SourceTextX alpha, OutputableBndrId alpha) => DynFlags -> (LHsDecl alpha, Maybe NDoc) -> [SimpleDescr]
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
#endif
extractMethod _ _ = []

extractMethodName :: (Outputable o1, Outputable o2
#if MIN_VERSION_ghc(8,8,0)
  , HasSrcSpan (GenLocated l o2)
#endif
  ) => DynFlags
                           -> SrcSpan -> o1 -> Maybe NDoc -> GenLocated l o2 -> SimpleDescr
extractMethodName dflags loc ts mbDoc name =
    SimpleDescr
        (T.pack . showSDoc dflags . ppr $ unLoc name)
        (Just . BS.pack . showSDocUnqual dflags $ ppr ts)
        (srcSpanToLocation loc)
        (toComment mbDoc [])
        True

extractConstructor
  :: DynFlags
  -> Located (ConDecl GhcPs)
  -> [SimpleDescr]
#if MIN_VERSION_ghc(8,0,0)
extractConstructor dflags decl@(L loc d') = extractDecl d'
 where
  extractDecl (ConDeclGADT {..}) = map (extractName con_doc) con_names
  extractDecl (ConDeclH98 {..}) = [extractName con_doc con_name]
#if MIN_VERSION_ghc(8,6,0)
  extractDecl (XConDecl {}) = []
#endif
#else
extractConstructor dflags decl@(L loc (ConDecl {con_names = names, con_doc = doc})) =
    map (extractName doc) names
 where
#endif
  extractName doc name =
    SimpleDescr
        (T.pack . showSDoc dflags . ppr $ unLoc name)
        (Just . BS.pack . showSDocUnqual dflags . ppr $ uncommentDecl decl)
        (srcSpanToLocation loc)
        (case doc of
            Nothing -> Nothing
            Just (L _ d) -> Just . BS.pack . T.unpack $ printHsDoc d)
        True

extractRecordFields :: DynFlags -> Located (ConDecl GhcPs) -> [SimpleDescr]
#if MIN_VERSION_ghc(8,6,0)
extractRecordFields dflags (L _ _decl@ConDeclH98 {con_args = RecCon flds}) =
#elif MIN_VERSION_ghc(8,0,0)
extractRecordFields dflags (L _ _decl@ConDeclH98 {con_details = RecCon flds}) =
#else
extractRecordFields dflags (L _ _decl@ConDecl {con_details = RecCon flds}) =
#endif
    concatMap extractRecordFields' (unLoc flds)
    where
#if MIN_VERSION_ghc(8,6,0)
    extractRecordFields' :: (Outputable (HsType name)) => LConDeclField name -> [SimpleDescr]
    extractRecordFields' (L _ (XConDeclField _)) = []
    extractRecordFields' (L _ _field@(ConDeclField _ names typ doc)) = map extractName names
#else
    extractRecordFields' :: (SourceTextX name, OutputableBndrId name) => LConDeclField name -> [SimpleDescr]
    extractRecordFields' (L _ _field@(ConDeclField names typ doc)) = map extractName names
#endif
      where
      extractName :: (Outputable (HsType name)) => LFieldOcc name -> SimpleDescr
      extractName name =
        SimpleDescr
            (T.pack . showSDoc dflags . ppr $ unLoc name)
            (Just . BS.pack . showSDocUnqual dflags $ ppr typ)
            (srcSpanToLocation $ getLoc name)
            (case doc of
                Nothing -> Nothing
                Just (L _ d) -> Just . BS.pack . T.unpack $ printHsDoc d)
            True
#if MIN_VERSION_ghc(8,0,0)
extractRecordFields dflags (L _ _decl@ConDeclGADT
#if MIN_VERSION_ghc(8,6,0)
        {con_names = names, con_res_ty = typ, con_doc = doc}) =
#else
        {con_names = names, con_type = typ, con_doc = doc}) =
#endif
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

attachComments :: [LSig GhcPs] -> [MyLDocDecl] -> [(LHsDecl GhcPs, Maybe NDoc)]
attachComments sigs docs = collectDocs $ sortByLoc
#if MIN_VERSION_ghc(8,10,2)
        (map (\ (L l i) -> L l (SigD NoExtField i)) sigs ++ map (\ (L l i) -> L l (DocD NoExtField i)) docs)
#elif MIN_VERSION_ghc(8,6,0)
        (map (\ (L l i) -> L l (SigD NoExt i)) sigs ++ map (\ (L l i) -> L l (DocD NoExt i)) docs)
#else
        (map (\ (L l i) -> L l (SigD i)) sigs ++ map (\ (L l i) -> L l (DocD i)) docs)
#endif

sigToByteString :: DynFlags -> [(NSig, Maybe NDoc)] -> Maybe ByteString
sigToByteString _ [] = Nothing
sigToByteString dflags [(sig,_)] = Just (BS.pack (showSDocUnqual dflags $ppr sig))
sigToByteString dflags ((sig,_):_) = Just (BS.pack (showSDocUnqual dflags $ppr sig))

srcSpanToLocation :: SrcSpan -> Maybe Location
srcSpanToLocation (RealSrcSpan span')
    =   Just (Location (unpackFS $ srcSpanFile span') (srcSpanStartLine span') (srcSpanStartCol span')
                 (srcSpanEndLine span') (srcSpanEndCol span'))
srcSpanToLocation _ = Nothing

toComment :: Maybe NDoc -> [NDoc] -> Maybe ByteString
toComment (Just c) _    =  Just . BS.pack . T.unpack $ printHsDoc c
toComment Nothing (c:_) =  Just . BS.pack . T.unpack $ printHsDoc c
toComment Nothing []    =  Nothing


{--
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [Data] []
collectParseInfoForDecl (l,st) ((Just (L loc (TyClD (TyFamily _ lid _ _)))), mbComment')
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [] []
collectParseInfoForDecl (l,st) ((Just (L loc (TyClD (ClassDecl _ lid _ _ _ _ _ _ )))), mbComment')
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [Class] []
--}
printHsDoc :: NDoc  -> Text
#if MIN_VERSION_ghc(8,6,0)
printHsDoc fs = T.pack $ unpackHDS fs
#else
printHsDoc (HsDocString fs) = T.pack $ unpackFS fs
#endif

---------------------------------------------------------------------------------
-- Now the interface file stuff

mayGetInterfaceFile :: PackageIdAndKey -> ModuleName -> Ghc (Maybe (ModIface,FilePath))
mayGetInterfaceFile p mn =
    let pid             =   packId p
#if MIN_VERSION_ghc(8,2,0)
        makeMod         =   mkModule (DefiniteUnitId (DefUnitId (packUnitId p)))
        makeInstMod     =   InstalledModule (packUnitId p)
#elif MIN_VERSION_ghc(8,0,0)
        makeMod         =   mkModule (packUnitId p)
#else
        makeMod         =   mkModule (packKey p)
#endif
        isBase  = pkgName pid == mkPackageName "base"
        mn'     = mkModuleName (display mn)
        iface   = findAndReadIface
                        empty
#if MIN_VERSION_ghc(8,2,0)
                        (makeInstMod mn')
#endif
                        (if isBase
                            then mkBaseModule_ mn'
                            else makeMod mn')
                        False
        gblEnv  =   IfGblEnv { if_rec_types = Nothing, if_doc = empty }
    in do
        hscEnv              <-  getSession
        maybe'              <-  liftIO $ initTcRnIf  'i' hscEnv gblEnv () iface
        case maybe' of
            M.Succeeded val ->    return (Just val)
            _               ->    return Nothing

mayGetInterfaceDescription :: DynFlags -> PackageIdAndKey -> ModuleName -> Ghc (Maybe ModuleDescr)
mayGetInterfaceDescription dflags pid mn = do
    mbIf <- mayGetInterfaceFile pid mn
    case mbIf of
        Nothing -> do
            liftIO $ debugM "leksah-server" ("no interface file for " ++ show mn)
            return Nothing
        Just (mif,_) ->
            let allDescrs  =    extractExportedDescrH dflags pid mif
                mod'       =    extractExportedDescrR dflags pid allDescrs mif
            in do
                liftIO $ debugM "leksah-server" ("interface file for " ++ show mn ++ " descrs: " ++
                                    show (length (mdIdDescriptions mod')))
                return (Just mod')



