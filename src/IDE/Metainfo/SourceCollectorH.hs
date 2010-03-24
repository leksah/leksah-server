{-# OPTIONS_GHC -XScopedTypeVariables -XBangPatterns #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Metainfo.SourceCollectorH
-- Copyright   :  2007-2010 Juergen Nicklisch-Franken, Hamish Mackenzie
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
,   interfaceToModuleDescr
,   PackageCollectStats(..)
) where

import IDE.Core.CTypes
       (PackageDescr(..), TypeDescr(..), RealDescr(..), Descr(..),
        ModuleDescr(..), PackModule(..), SimpleDescr(..), packageIdentifierToString)

#ifdef MIN_VERSION_haddock_leksah
import Haddock.Types
       (ExportItem(..), DeclInfo,
        Interface(..))
import Haddock.Interfaces
#else
import Documentation.Haddock
#endif
import Distribution.Text (simpleParse)
import InstEnv (Instance(..))
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
import System.Log.Logger (warningM, debugM)
import Control.DeepSeq (deepseq)
import Data.ByteString.Char8 (ByteString)
import Outputable hiding (trace)
import GHC.Show(showSpace)

getThisPackage :: PackageConfig -> PackageIdentifier
#if MIN_VERSION_Cabal(1,8,0)
getThisPackage    =   IPI.sourcePackageId
#else
getThisPackage    =   IPI.package
#endif

#ifdef MIN_VERSION_haddock_leksah
#else
type HsDoc = Doc
#endif

type NDoc  = HsDoc Name

isEmptyDoc :: NDoc -> Bool
isEmptyDoc DocEmpty  = True
isEmptyDoc _         = False

show' :: Outputable alpha => alpha  -> String
#if MIN_VERSION_ghc(6,12,1)
type MyLDocDecl = LDocDecl
show' = showSDoc . ppr
#else
type MyLDocDecl = LDocDecl Name
show' =  showSDoc . ppr
#endif

data PackageCollectStats = PackageCollectStats {
    packageString       :: String,
    modulesTotal        :: Maybe Int,
    withSource          :: Bool,
    retrieved           :: Bool,
    mbError             :: Maybe String}

-- Hell

collectPackageFromSource :: Prefs -> PackageConfig -> IO (Maybe PackageDescr, PackageCollectStats, Maybe FilePath)
collectPackageFromSource prefs packageConfig = do
    sourceMap <- liftIO $ getSourcesMap prefs
    case sourceForPackage (getThisPackage packageConfig) sourceMap of
        Just fp -> do
            let dirPath      = dropFileName fp
            setCurrentDirectory dirPath
            runTool' "cabal" (["configure","--user"]) Nothing
            packageFromSource fp packageConfig
        Nothing -> do
            unpackDir <- getUnpackDirectory prefs
            case unpackDir of
                Nothing -> return (Nothing, PackageCollectStats packageName Nothing False False
                                (Just ("No source found. Prefs don't allow for retreiving")), Nothing)
                Just fp -> do
                    exists <- doesDirectoryExist fp
                    unless exists $ createDirectory fp
                    setCurrentDirectory fp
                    runTool' "cabal" (["unpack",packageName]) Nothing
                    success <- doesDirectoryExist (fp </> packageName)
                    if not success
                        then return (Nothing, PackageCollectStats packageName Nothing False False
                                (Just ("Failed to download and unpack source")), Nothing)
                        else do
                            setCurrentDirectory (fp </> packageName)
                            runTool' "cabal" (["configure","--user"]) Nothing
                            packageFromSource (fp </> packageName </> packageName <.> "cabal")
                                        packageConfig
    where
        packageName = packageIdentifierToString (getThisPackage packageConfig)

packageFromSource :: FilePath -> PackageConfig -> IO (Maybe PackageDescr, PackageCollectStats, Maybe FilePath)
packageFromSource cabalPath packageConfig = do
    setCurrentDirectory dirPath
    ghcFlags <- figureOutHaddockOpts
    debugM "leksah-server" ("ghcFlags:  " ++ show ghcFlags)
    NewException.catch (inner ghcFlags) handler
    where
        _handler' (_e :: NewException.SomeException) =
            trace "would block" $ return ([])
        handler (e :: NewException.SomeException) = do
            warningM "leksah-server" ("Ghc failed to process: " ++ show e)
            return (Nothing, PackageCollectStats packageName Nothing False False
                                            (Just ("Ghc failed to process: " ++ show e)), Just dirPath)
        inner ghcFlags = inGhcIO ghcFlags [Opt_Haddock] $ \ _flags -> do
            (interfaces,_) <- createInterfaces verbose (exportedMods ++ hiddenMods) [] []
            liftIO $ print (length interfaces)
            let mods = map (interfaceToModuleDescr dirPath (getThisPackage packageConfig)) interfaces
            sp <- liftIO $ canonicalizePath dirPath
            let pd = PackageDescr {
                    pdPackage           =   getThisPackage packageConfig
                ,   pdModules           =   mods
                ,   pdBuildDepends      =   [] -- TODO depends packageConfig
                ,   pdMbSourcePath      =   Just sp}
            let stat = PackageCollectStats packageName (Just (length mods)) True False Nothing
            liftIO $ deepseq pd $ return (Just pd, stat, Just dirPath)
        exportedMods = map moduleNameString $ IPI.exposedModules packageConfig
        hiddenMods   = map moduleNameString $ IPI.hiddenModules packageConfig
        dirPath      = dropFileName cabalPath
        packageName  = packageIdentifierToString (getThisPackage packageConfig)

-- Heaven

interfaceToModuleDescr :: FilePath -> PackageIdentifier -> Interface -> ModuleDescr
interfaceToModuleDescr _dirPath pid interface =
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
                        (ifaceInstances interface) [] --(ifaceLocals interface)
        imports    = Map.empty --TODO

#if MIN_VERSION_ghc(6,12,1)
extractDescrs :: PackModule -> Map Name DeclInfo -> [ExportItem Name] -> [Instance] -> [Name] -> [Descr]
extractDescrs pm _ifaceDeclMap ifaceExportItems' ifaceInstances' _ifaceLocals =
	transformToDescrs pm exportedDeclInfo ++ map (toDescrInst pm) ifaceInstances'
    where
        exportedDeclInfo                               =  mapMaybe toDeclInfo  ifaceExportItems'
        toDeclInfo (ExportDecl decl mbDoc subDocs _)   =
                                        Just(decl,fst mbDoc,map (\ (a,b) -> (a,fst b)) subDocs)
        toDeclInfo (ExportNoDecl _ _)                  = Nothing
        toDeclInfo (ExportGroup _ _ _)                 = Nothing
        toDeclInfo (ExportDoc _)                       = Nothing
        toDeclInfo (ExportModule _)                    = Nothing
#else
extractDescrs :: PackModule -> Map Name DeclInfo -> [ExportItem Name] -> [Instance] -> [Name] -> [Descr]
extractDescrs pm _ifaceDeclMap ifaceExportItems' ifaceInstances' _ifaceLocals =
	transformToDescrs pm exportedDeclInfo ++ map (toDescrInst pm) ifaceInstances'
    where
        exportedDeclInfo                               =  mapMaybe toDeclInfo  ifaceExportItems'
        toDeclInfo (ExportDecl decl mbDoc subDocs _)   = Just(decl,mbDoc,subDocs)
        toDeclInfo (ExportNoDecl _ _)                  = Nothing
        toDeclInfo (ExportGroup _ _ _)                 = Nothing
        toDeclInfo (ExportDoc _)                       = Nothing
        toDeclInfo (ExportModule _)                    = Nothing
#endif

transformToDescrs :: PackModule -> [(LHsDecl Name, Maybe NDoc, [(Name, Maybe NDoc)])] -> [Descr]
transformToDescrs pm = concatMap transformToDescr
    where
    transformToDescr ((L loc (SigD (TypeSig name typ))), mbComment,_subCommentList) =
        [Real $ RealDescr {
        dscName'        =   getOccString (unLoc name)
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual $ppr typ))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   VariableDescr
    ,   dscExported'    =   True}]

    transformToDescr ((L _loc (SigD _)), _mbComment, _subCommentList) = []
    transformToDescr ((L loc (TyClD typ@(TySynonym lid _ _ _ ))), mbComment, _subCommentList) =
        [Real $ RealDescr {
        dscName'        =   getOccString (unLoc lid)
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual $ppr typ))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]

    transformToDescr ((L loc (TyClD typ@(TyData DataType _ tcdLName' _ _ _ lConDecl tcdDerivs'))), mbComment,_subCommentList) =
        [Real $ RealDescr {
        dscName'        =   name
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual $ppr (uncommentData typ)))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   DataDescr constructors fields
    ,   dscExported'    =   True}]
            ++ derivings tcdDerivs'
        where
        constructors    =   map extractConstructor lConDecl
        fields          =   nub $ concatMap extractRecordFields lConDecl
        name            =   getOccString (unLoc tcdLName')
        derivings Nothing = []
        derivings (Just _l) = []

    transformToDescr ((L loc (TyClD typ@(TyData NewType _ tcdLName' _ _ _ lConDecl tcdDerivs'))), mbComment,_subCommentList) =
        [Real $ RealDescr {
        dscName'        =   name
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual $ppr (uncommentData typ)))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   NewtypeDescr constructor mbField
    ,   dscExported'    =   True}]
        ++ derivings tcdDerivs'
        where
        constructor     =   forceHead (map extractConstructor lConDecl)
                                "WorkspaceCollector>>transformToDescr: no constructor for newtype"
        mbField         =   case concatMap extractRecordFields lConDecl of
                                [] -> Nothing
                                a:_ -> Just a
        name            =   getOccString (unLoc tcdLName')
        derivings Nothing = []
        derivings (Just _l) = []

    transformToDescr ((L loc (TyClD cl@(ClassDecl _ tcdLName' _ _ tcdSigs' _ _ docs))), mbComment,_subCommentList) =
        [Real $ RealDescr {
        dscName'        =   getOccString (unLoc tcdLName')
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual $ppr cl{tcdMeths = emptyLHsBinds}))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation loc
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   ClassDescr super methods
    ,   dscExported'    =   True    }]
        where
        methods         =   extractMethods tcdSigs' docs
        super           =   []

    transformToDescr (_, _mbComment, _sigList) = []

toDescrInst :: PackModule -> Instance -> Descr
toDescrInst pm inst@(Instance is_cls' _is_tcs _is_tvs is_tys' _is_dfun _is_flag) =
        Real $ RealDescr {
        dscName'        =   getOccString is_cls'
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual $ppr inst))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   srcSpanToLocation (getSrcSpan inst)
    ,   dscMbComment'   =   Nothing
    ,   dscTypeHint'    =   InstanceDescr (map (showSDocUnqual . ppr) is_tys')
    ,   dscExported'    =   True}

extractMethods :: [LSig Name] -> [MyLDocDecl] -> [SimpleDescr]
extractMethods sigs docs =
    let pairs = attachComments' sigs docs
    in mapMaybe extractMethod pairs

extractMethod :: (LHsDecl Name, Maybe NDoc) -> Maybe SimpleDescr
extractMethod ((L loc (SigD ts@(TypeSig name _typ))), mbDoc) =
    Just $ SimpleDescr
        (getOccString (unLoc name))
        (Just (BS.pack (showSDocUnqual $ ppr ts)))
        (srcSpanToLocation loc)
        (toComment mbDoc [])
        True
extractMethod (_, _mbDoc) = Nothing

extractConstructor :: LConDecl Name -> SimpleDescr
extractConstructor decl@(L loc (ConDecl {con_name = name, con_doc = doc})) =
    SimpleDescr
        (getOccString (unLoc name))
        (Just (BS.pack (showSDocUnqual $ppr (uncommentDecl decl))))
        (srcSpanToLocation loc)
        (case doc of
            Nothing -> Nothing
            Just (L _ d) -> Just (BS.pack (printHsDoc'' d)))
        True

extractRecordFields :: LConDecl Name -> [SimpleDescr]
extractRecordFields (L _ _decl@(ConDecl {con_details=(RecCon flds)})) =
    map extractRecordFields' flds
    where
    extractRecordFields' _field@(ConDeclField (L loc name) typ doc) =
        SimpleDescr
            (getOccString name)
            (Just (BS.pack (showSDocUnqual $ ppr typ)))
            (srcSpanToLocation loc)
            (case doc of
                Nothing -> Nothing
                Just (L _ d) -> Just (BS.pack (printHsDoc'' d)))
            True
extractRecordFields _ = []

toComment :: Maybe NDoc -> [NDoc] -> Maybe ByteString
toComment (Just c) _    =  Just (BS.pack (printHsDoc' c))
toComment Nothing (c:_) =  Just (BS.pack (printHsDoc' c))
toComment Nothing []    =  Nothing


{--
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [Data] []
collectParseInfoForDecl (l,st) ((Just (L loc (TyClD (TyFamily _ lid _ _)))), mbComment')
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [] []
collectParseInfoForDecl (l,st) ((Just (L loc (TyClD (ClassDecl _ lid _ _ _ _ _ _ )))), mbComment')
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [Class] []
--}

printHsDoc' :: HsDoc Name  -> String
printHsDoc' d = show (PPDoc d)

#if MIN_VERSION_ghc(6,12,1)
printHsDoc'' :: HsDocString  -> String
printHsDoc''  = printHsDoc
#else
printHsDoc'' :: HsDoc Name  -> String
printHsDoc''  = printHsDoc'
#endif

newtype PPDoc alpha = PPDoc (HsDoc alpha)

instance Outputable alpha => Show (PPDoc alpha)  where
    showsPrec _ (PPDoc DocEmpty)                 =   id
    showsPrec _ (PPDoc (DocAppend l r))          =   shows (PPDoc l)  . shows (PPDoc r)
    showsPrec _ (PPDoc (DocString str))          =   showString str
    showsPrec _ (PPDoc (DocParagraph d))         =   shows (PPDoc d) . showChar '\n'
    showsPrec _ (PPDoc (DocIdentifier l))        =   foldr (\i _f -> showChar '\'' .
                                                     ((showString . showSDoc .  ppr) i) . showChar '\'') id l
    showsPrec _ (PPDoc (DocModule str))          =   showChar '"' . showString str . showChar '"'
    showsPrec _ (PPDoc (DocEmphasis doc))        =   showChar '/' . shows (PPDoc doc)  . showChar '/'
    showsPrec _ (PPDoc (DocMonospaced doc))      =   showChar '@' . shows (PPDoc doc) . showChar '@'
    showsPrec _ (PPDoc (DocUnorderedList l))     =
        foldr (\s r -> showString "* " . shows (PPDoc s) . showChar '\n' . r) id l
    showsPrec _ (PPDoc (DocOrderedList l))       =
        foldr (\(i,n) _f -> shows n . showSpace .  shows (PPDoc i)) id (zip l [1 .. length l])
    showsPrec _ (PPDoc (DocDefList li))          =
        foldr (\(l,r) f -> showString "[@" . shows (PPDoc l) . showString "[@ " . shows (PPDoc r) . f) id li
    showsPrec _ (PPDoc (DocCodeBlock doc))      =   showChar '@' . shows (PPDoc doc) . showChar '@'
    showsPrec _ (PPDoc (DocURL str))            =   showChar '<' . showString str . showChar '>'
    showsPrec _ (PPDoc (DocAName str))          =   showChar '#' . showString str . showChar '#'
    showsPrec _ (PPDoc _)                       =   id

attachComments' :: [LSig Name] -> [MyLDocDecl] -> [(LHsDecl Name, Maybe (HsDoc Name))]
attachComments' sigs docs = collectDocs' $ sortByLoc $
        ((map (\ (L l i) -> L l (SigD i)) sigs) ++ (map (\ (L l i) -> L l (DocD i)) docs))

-- | Collect the docs and attach them to the right declaration.
collectDocs' :: [LHsDecl Name] -> [(LHsDecl Name, (Maybe (HsDoc Name)))]
collectDocs' = collect' Nothing DocEmpty

collect' :: Maybe (LHsDecl Name) -> HsDoc Name -> [LHsDecl Name] -> [(LHsDecl Name, (Maybe (HsDoc Name)))]
collect' d doc_so_far [] =
   case d of
        Nothing -> []
        Just d0  -> finishedDoc' d0 doc_so_far []

collect' d doc_so_far (e:es) =
  case e of
    L _ (DocD (DocCommentNext str)) ->
      case d of
        Nothing -> collect' d (DocAppend doc_so_far (DocString (show' str))) es
        Just d0 -> finishedDoc' d0 doc_so_far (collect' Nothing (DocString (show' str)) es)

    L _ (DocD (DocCommentPrev str)) -> collect' d (DocAppend doc_so_far (DocString (show' str))) es

    _ -> case d of
      Nothing -> collect' (Just e) doc_so_far es
      Just d0 -> finishedDoc' d0 doc_so_far (collect' (Just e) DocEmpty es)

finishedDoc' :: LHsDecl alpha -> NDoc -> [(LHsDecl alpha, (Maybe ((HsDoc Name))))]
                    -> [(LHsDecl alpha, (Maybe ((HsDoc Name))))]
finishedDoc' d doc rest | isEmptyDoc doc = (d, Nothing) : rest
finishedDoc' d doc rest | notDocDecl d   = (d, Just doc) : rest
  where
    notDocDecl (L _ (DocD _)) = False
    notDocDecl _              = True
finishedDoc' _ _ rest = rest
