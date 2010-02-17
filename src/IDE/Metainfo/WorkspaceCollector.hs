{-# OPTIONS_GHC -XScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Metainfo.WorkspaceCollector
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

module IDE.Metainfo.WorkspaceCollector (

    collectWorkspace

,   uncommentData
,   toComment
,   attachComments
,   uncommentDecl
,   printHsDoc
,   srcSpanToLocation
,   sigToByteString


) where


import IDE.Utils.Utils
import IDE.Utils.GHCUtils
import GHC hiding(Id,Failed,Succeeded,ModuleName)
import HscTypes hiding (liftIO)
import Outputable hiding(trace)
import ErrUtils
import Debug.Trace
import qualified Data.Map as Map
import Data.Map(Map)
import System.Directory
import Distribution.Package hiding (PackageId)
import Distribution.ModuleName
import Distribution.Text (simpleParse)
import Control.Monad.Reader
import System.IO
import System.FilePath
import System.Directory
import qualified Data.ByteString.Char8 as BS
import Data.Binary.Shared
import IDE.Utils.FileUtils
import IDE.Core.Serializable ()
import IDE.Utils.Utils
import IDE.Core.CTypes hiding (SrcSpan(..))
import Data.ByteString.Char8 (ByteString(..))
import DriverPipeline (preprocess)
import System.Directory
import StringBuffer(hGetStringBuffer)
import Data.List(partition,sortBy,nub,find)
import Data.Ord(comparing)
import RdrName (showRdrName)
import GHC.Show(showSpace)
import GHC.Exception
import MyMissing(forceHead)
import LoadIface(findAndReadIface)
import Distribution.Text(display)
import TcRnMonad hiding (liftIO,MonadIO,LIE)
import qualified Maybes as M
import IDE.Metainfo.InterfaceCollector
import Data.Maybe
       (isJust, fromJust, catMaybes, mapMaybe, isNothing)
import Module (stringToPackageId)
import PrelNames
--import FastString (appendFS,nilFS)
import System.Log.Logger
--import Data.Maybe (isJust)

type NDecl = LHsDecl RdrName
type NDoc  = HsDoc RdrName
type NSig  = Located (Sig RdrName)

collectWorkspace :: PackageIdentifier ->  [(String,FilePath)] -> Bool -> Bool -> FilePath -> IO()
collectWorkspace packId moduleList forceRebuild writeAscii dir = do
    debugM "leksah-server" $ "collectWorkspace called with " ++ show moduleList
    collectorPath <- liftIO $ getCollectorPath
    let packageCollectorPath = collectorPath </> packageIdentifierToString packId
    when forceRebuild $ do
        exists <- doesDirectoryExist packageCollectorPath
        when exists $ removeDirectoryRecursive packageCollectorPath
    -- Construct directory
    liftIO $ createDirectoryIfMissing True packageCollectorPath
    setCurrentDirectory dir
    opts <- figureOutGhcOpts
    debugM "leksah-server" $ "before collect modules"
    mapM_ (collectModule packageCollectorPath writeAscii packId opts) moduleList
    debugM "leksah-server" $ "after collect modules"

collectModule :: FilePath -> Bool -> PackageIdentifier -> [String] -> (String,FilePath) -> IO()
collectModule collectorPackagePath writeAscii packId opts (modId,sourcePath) = do
    existCollectorFile <- doesFileExist collectorModulePath
    existSourceFile    <- doesFileExist sourcePath
    case mbModuleName of
        Nothing -> errorM "leksah-server" ("Can't parse module name " ++ modId)
        Just moduleName ->
            if existSourceFile
            then do
                if not existCollectorFile
                    then collectModule' sourcePath collectorModulePath writeAscii packId opts moduleName
                    else do
                        sourceModTime <-  getModificationTime sourcePath
                        collModTime   <-  getModificationTime collectorModulePath
                        if sourceModTime > collModTime
                            then collectModule' sourcePath collectorModulePath writeAscii packId
                                    opts moduleName
                            else return ()
            else errorM "leksah-server" ("source file not found " ++ sourcePath)
    where
        collectorModulePath = collectorPackagePath </> modId <.> leksahMetadataWorkspaceFileExtension
        mbModuleName = simpleParse modId


collectModule' :: FilePath -> FilePath -> Bool -> PackageIdentifier -> [String] -> ModuleName -> IO()
collectModule' sourcePath destPath writeAscii packId opts moduleName =
    trace ("now collect module " ++ show sourcePath ++ " opts: " ++ show opts) $
    gcatch (
    inGhcIO opts [Opt_Haddock,Opt_Cpp] $ \ dynFlags -> do
        session         <-  getSession
        (dynFlags3,fp') <-  preprocess session (sourcePath,Nothing)
        mbInterfaceDescr <- mayGetInterfaceDescription packId moduleName
        liftIO $ do
            stringBuffer    <-  hGetStringBuffer fp'
            parseResult     <-  myParseModule dynFlags3 sourcePath (Just stringBuffer)
            case parseResult of
                Right (L _ hsMod@(HsModule _ _ _ decls _ _ _ )) -> do
                    let moduleDescr = extractModDescr packId moduleName sourcePath hsMod
                    let moduleDescr' = case mbInterfaceDescr of
                                            Nothing -> moduleDescr
                                            Just md  -> mergeWithInterfaceDescr moduleDescr md
                    catch (writeExtractedModule destPath writeAscii moduleDescr')
                        (\ _ -> errorM "leksah-server" ("Can't write extracted package " ++ destPath))
                Left errMsg -> do
                    errorM "leksah-server" $ "Failed to parse " ++ sourcePath ++ " " ++ show errMsg
                    let moduleDescr =  ModuleDescr {
                        mdModuleId          =   PM packId moduleName
                    ,   mdMbSourcePath      =   Just sourcePath
                    ,   mdReferences        =   Map.empty -- imports
                    ,   mdIdDescriptions    =   [Real $ RealDescr {
                            dscName'        =   "Parse Error"
                        ,   dscMbTypeStr'   =   Nothing
                        ,   dscMbModu'      =   Just (PM packId moduleName)
                        ,   dscMbLocation'  =   case errMsgSpans errMsg of
                                                    (sp:_) -> Just (srcSpanToLocation sp)
                                                    [] -> Nothing
                        ,   dscMbComment'   =   Just (BS.pack $ show errMsg)
                        ,   dscTypeHint'    =   ErrorDescr
                        ,   dscExported'    =   False}]}
                    catch (writeExtractedModule destPath writeAscii moduleDescr)
                        (\ _ -> errorM "leksah-server" ("Can't write extracted module " ++ destPath))
    ) (\ (e :: SomeException) -> errorM "leksah-server" ("Can't extract module " ++ destPath ++ " " ++ show e))


writeExtractedModule :: MonadIO m => FilePath -> Bool -> ModuleDescr -> m ()
writeExtractedModule filePath writeAscii md =
    if writeAscii
        then liftIO $ writeFile (filePath ++ "dpg") (show md)
        else liftIO $ encodeFileSer filePath (metadataVersion, md)

-----------------------------------------------------------------------------------
-- Format conversion

extractModDescr :: PackageIdentifier -> ModuleName -> FilePath -> HsModule RdrName -> ModuleDescr
extractModDescr packId moduleName sourcePath hsMod = ModuleDescr {
        mdModuleId          =   PM packId moduleName
    ,   mdMbSourcePath      =   Just sourcePath
    ,   mdReferences        =   Map.empty -- imports
    ,   mdIdDescriptions    =   descrs'}
    where
        descrs = extractDescrs (PM packId moduleName) (hsmodDecls hsMod)
        descrs' = fixExports (hsmodExports hsMod) descrs

-----------------------------------------------------------------------------------
-- Add exported hint

fixExports :: Maybe [LIE RdrName] -> [Descr] -> [Descr]
fixExports Nothing descrs = descrs
fixExports (Just iel) descrs = map (fixDescr (map unLoc iel)) descrs
    where
        fixDescr ::  [IE RdrName] -> Descr -> Descr
        fixDescr _ d@(Reexported _) = d
        fixDescr list (Real rd) = Real rd'
            where
                rd' = case dscTypeHint' rd of
                          VariableDescr   -> rd{dscExported' = isJust findVar}
                          InstanceDescr _ -> rd
                          otherwise       -> case findThing of
                                                Nothing                -> nothingExported rd
                                                Just (IEThingAll _)    -> allExported rd
                                                Just (IEThingAbs _)    -> someExported rd []
                                                Just (IEThingWith _ l) -> someExported rd (map showRdrName l)
                                                _                      -> allExported rd
                findVar = find (\ a ->
                            case a of
                                IEVar r | showRdrName r == dscName' rd -> True
                                _                                     -> False)
                                    list
                findThing = find (\ a ->
                                case a of
                                IEThingAbs r | showRdrName r == dscName' rd -> True
                                IEThingAll r | showRdrName r == dscName' rd -> True
                                IEThingWith r list | showRdrName r == dscName' rd -> True
                                _                                     -> False)
                                    list
        allExported rd                                 = rd
        nothingExported rd                             = rd{dscExported' = False,
                                                             dscTypeHint' = nothingExportedS (dscTypeHint' rd)}
        nothingExportedS (DataDescr lsd1 lsd2)         = DataDescr (map (setExportedSD False) lsd1)
                                                            (map (setExportedSD False) lsd2)
        nothingExportedS (NewtypeDescr sd1 Nothing)    = NewtypeDescr (setExportedSD False sd1)
                                                            Nothing
        nothingExportedS (NewtypeDescr sd1 (Just sd2)) = NewtypeDescr (setExportedSD False sd1)
                                                            (Just (setExportedSD False sd1))
        nothingExportedS (ClassDescr n lsd2)           = ClassDescr n (map (setExportedSD False) lsd2)
        nothingExportedS other                         = other

        someExported rd l                              = rd{dscExported' = True,
                                                            dscTypeHint' = someExportedS (dscTypeHint' rd) l}
        someExportedS (DataDescr lsd1 lsd2) l          = DataDescr (map (maySetExportedSD l) lsd1)
                                                            (map (maySetExportedSD l) lsd2)
        someExportedS (NewtypeDescr sd1 Nothing) l     = NewtypeDescr (maySetExportedSD l sd1)
                                                            Nothing
        someExportedS (NewtypeDescr sd1 (Just sd2)) l  = NewtypeDescr (maySetExportedSD l sd1)
                                                            (Just (maySetExportedSD l sd1))
        someExportedS (ClassDescr n lsd2) l            = ClassDescr n (map (maySetExportedSD l) lsd2)
        someExportedS other _                          = other


        setExportedSD bool sd = sd{sdExported = bool}
        maySetExportedSD list sd = sd{sdExported = elem (sdName sd) list}


extractDescrs :: PackModule -> [NDecl] -> [Descr]
extractDescrs pm decls = transformToDescrs pm tripleWithSigs
    where
        sortedDecls                    = sortByLoc decls
        pairedWithDocs                 = collectDocs sortedDecls
        filteredDecls                  = filterUninteresting pairedWithDocs
        (withoutSignatures,signatures) = partitionSignatures filteredDecls
        tripleWithSigs                 = attachSignatures signatures withoutSignatures

-- | Sort by source location
sortByLoc :: [Located a] -> [Located a]
sortByLoc = sortBy (comparing getLoc)

filterUninteresting :: [(NDecl,Maybe NDoc)] -> [(NDecl,Maybe NDoc)]
filterUninteresting = filter filterSignature
    where
    filterSignature ((L srcDecl (SpliceD _)),_)  = False
    filterSignature ((L srcDecl (RuleD _)),_)    = False
    filterSignature ((L srcDecl (WarningD _)),_) = False
    filterSignature ((L srcDecl (ForD _)),_)     = False
    filterSignature ((L srcDecl (DefD _)),_)     = False
    filterSignature _                            = True

partitionSignatures :: [(NDecl,Maybe NDoc)] -> ([(NDecl,Maybe NDoc)],[(NDecl,Maybe NDoc)])
partitionSignatures = partition filterSignature
    where
    filterSignature ((L srcDecl (SigD _)),_) = False
    filterSignature _ = True

partitionInstances :: [(NDecl,Maybe NDoc)] -> ([(NDecl,Maybe NDoc)],[(NDecl,Maybe NDoc)])
partitionInstances i = (i,[])
--partition filterInstances
--    where
--    filterInstances ((L srcDecl (InstD _)),_) = False
--    filterInstances _ = True

-- | Collect the docs and attach them to the right declaration.
collectDocs :: [LHsDecl alpha] -> [(LHsDecl alpha, (Maybe (HsDoc alpha)))]
collectDocs = collect Nothing DocEmpty

collect :: Maybe (LHsDecl alpha) -> HsDoc alpha -> [LHsDecl alpha] -> [(LHsDecl alpha, (Maybe (HsDoc alpha)))]
collect d doc_so_far [] =
   case d of
        Nothing -> []
        Just d0  -> finishedDoc d0 doc_so_far []

collect d doc_so_far (e:es) =
  case e of
    L _ (DocD (DocCommentNext str)) ->
      case d of
        Nothing -> collect d (docAppend doc_so_far str) es
        Just d0 -> finishedDoc d0 doc_so_far (collect Nothing str es)

    L _ (DocD (DocCommentPrev str)) -> collect d (docAppend doc_so_far str) es

    _ -> case d of
      Nothing -> collect (Just e) doc_so_far es
      Just d0 -> finishedDoc d0 doc_so_far (collect (Just e) DocEmpty es)

finishedDoc :: LHsDecl alpha -> HsDoc alpha -> [(LHsDecl alpha, (Maybe (HsDoc alpha)))] -> [(LHsDecl alpha, (Maybe (HsDoc alpha)))]
finishedDoc d DocEmpty rest = (d, Nothing) : rest
finishedDoc d doc rest | notDocDecl d = (d, Just doc) : rest
  where
    notDocDecl (L _ (DocD _)) = False
    notDocDecl _              = True
finishedDoc _ _ rest = rest

attachSignatures :: [(NDecl, (Maybe NDoc))] -> [(NDecl,Maybe NDoc)]
    -> [(NDecl, (Maybe NDoc), [(NSig,Maybe NDoc)])]
attachSignatures signatures = map (attachSignature signaturesMap)
    where
    signaturesMap = Map.fromListWith (++)
                        $ map (\ (L loc (SigD sig),c) -> (fromJust $ sigNameNoLoc sig, [(L loc sig,c)]))
                                signatures
    attachSignature :: Map RdrName  [(NSig,Maybe NDoc)] -> (NDecl, (Maybe NDoc))
        -> (NDecl, (Maybe NDoc), [(NSig,Maybe NDoc)])
    attachSignature signaturesMap  (decl,mbDoc) =
        case declName (unLoc decl) of
            Nothing -> (decl,mbDoc, [])
            Just name -> case name `Map.lookup` signaturesMap of
                            Just sigList -> (decl,mbDoc, sigList)
                            Nothing ->  (decl, mbDoc, [])
    declName t@(TyClD x)                          = Just (tcdName x)
    declName t@(ValD (FunBind fun_id _ _ _ _ _ )) = Just (unLoc fun_id)
    declName _                                    = Nothing


transformToDescrs :: PackModule -> [(NDecl, (Maybe NDoc), [(NSig, Maybe NDoc)])] -> [Descr]
transformToDescrs pm = trace ("transformToDescrs " ++ show pm) $ concatMap transformToDescr
    where
    transformToDescr :: (NDecl, (Maybe NDoc), [(NSig, Maybe NDoc)]) -> [Descr]
    transformToDescr ((L loc (ValD (FunBind lid _ _ _ _ _))), mbComment,sigList) =
        [Real $ RealDescr {
        dscName'        =   showRdrName (unLoc lid)
    ,   dscMbTypeStr'   =   sigToByteString sigList
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   Just (srcSpanToLocation loc)
    ,   dscMbComment'   =   toComment mbComment (catMaybes (map snd sigList))
    ,   dscTypeHint'    =   VariableDescr
    ,   dscExported'    =   True}]

    transformToDescr ((L loc (TyClD typ@(TySynonym lid _ _ _ ))), mbComment,sigList) =
        [Real $ RealDescr {
        dscName'        =   showRdrName (unLoc lid)
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual $ppr typ))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   Just (srcSpanToLocation loc)
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   TypeDescr
    ,   dscExported'    =   True}]

    transformToDescr ((L loc (TyClD typ@(TyData DataType _ tcdLName _ _ _ lConDecl tcdDerivs))), mbComment,sigList) =
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
        name            =   showRdrName (unLoc tcdLName)
        derivings Nothing = []
        derivings (Just l) = map (extractDeriving pm name) l

    transformToDescr ((L loc (TyClD typ@(TyData NewType _ tcdLName _ _ _ lConDecl tcdDerivs))), mbComment,sigList) =
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
        name            =   showRdrName (unLoc tcdLName)
        derivings Nothing = []
        derivings (Just l) = map (extractDeriving pm name) l

    transformToDescr ((L loc (TyClD cl@(ClassDecl _ tcdLName _ _ tcdSigs _ _ docs))), mbComment,sigList) =
        [Real $ RealDescr {
        dscName'        =   showRdrName (unLoc tcdLName)
    ,   dscMbTypeStr'   =   Just (BS.pack (showSDocUnqual $ppr cl{tcdMeths = emptyLHsBinds}))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   Just (srcSpanToLocation loc)
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   ClassDescr super methods
    ,   dscExported'    =   True    }]
        where
        methods         =   extractMethods tcdSigs docs
        super           =   []

    transformToDescr ((L loc (InstD inst@(InstDecl typ _ _ _))), mbComment, sigList) =
        [Real $ RealDescr {
        dscName'        =   name
    ,   dscMbTypeStr'   =   Just (BS.pack ("instance " ++ (showSDocUnqual $ppr typ)))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   Just (srcSpanToLocation loc)
    ,   dscMbComment'   =   toComment mbComment []
    ,   dscTypeHint'    =   InstanceDescr other
    ,   dscExported'    =   True}]
        where
        (name,other)           =   case words (showSDocUnqual $ppr typ) of
                                [] -> ("",[])
                                hd:tl -> (hd,tl)

    transformToDescr (_, mbComment,sigList) = []


uncommentData :: TyClDecl a -> TyClDecl a
uncommentData (TyData a1 a2 a3 a4 a5 a6 conDecls a8) =
    TyData a1 a2 a3 a4 a5 a6 (map uncommentDecl conDecls) Nothing
uncommentData other                                  = other

uncommentDecl :: LConDecl a -> LConDecl a
uncommentDecl (L l (ConDecl a1 a2 a3 a4 fields a6 _)) =
    L l (ConDecl a1 a2 a3 a4 (uncommentDetails fields) a6 Nothing)

uncommentDetails :: HsConDeclDetails a -> HsConDeclDetails a
uncommentDetails (RecCon flds) = RecCon (map uncommentField flds)
    where
    uncommentField (ConDeclField a1 a2 doc)  =  ConDeclField a1 a2 Nothing
uncommentDetails other = other

mergeWithInterfaceDescr :: ModuleDescr -> ModuleDescr -> ModuleDescr
mergeWithInterfaceDescr md imd = md {
    mdReferences = mdReferences imd,
    mdIdDescriptions = mergeIdDescrs (mdIdDescriptions md) (mdIdDescriptions imd)}

mergeIdDescrs :: [Descr] -> [Descr] -> [Descr]
mergeIdDescrs d1 d2 = dres ++ reexported
    where
        (reexported,real)  = partition isReexported d2
        lm = Map.fromList $ zip (map (\d -> (dscName d,dscTypeHint d)) real) real
        dres =  map (addType lm) d1

        addType lm (Real d1) | isNothing (dscMbTypeStr' d1) =
            Real $ d1{dscMbTypeStr' = case (dscName' d1, dscTypeHint' d1) `Map.lookup` lm of
                                        Nothing -> Nothing
                                        Just d -> dscMbTypeStr d}
        addType _ d                     = d

extractDeriving :: OutputableBndr alpha => PackModule -> String -> LHsType alpha -> Descr
extractDeriving pm name (L loc typ) =
        Real $ RealDescr {
        dscName'        =   className
    ,   dscMbTypeStr'   =   Just (BS.pack ("instance " ++ (className ++ " " ++ name)))
    ,   dscMbModu'      =   Just pm
    ,   dscMbLocation'  =   Just (srcSpanToLocation loc)
    ,   dscMbComment'   =   toComment (Nothing :: Maybe NDoc) []
    ,   dscTypeHint'    =   InstanceDescr (words name)
    ,   dscExported'    =   True}
        where
        className       =   showSDocUnqual $ ppr typ

extractMethods :: OutputableBndr alpha => [LSig alpha] -> [LDocDecl alpha] -> [SimpleDescr]
extractMethods sigs docs =
    let pairs = attachComments sigs docs
    in mapMaybe extractMethod pairs

extractMethod :: OutputableBndr alpha => (LHsDecl alpha, Maybe (HsDoc alpha)) -> Maybe SimpleDescr
extractMethod ((L loc (SigD ts@(TypeSig name typ))), mbDoc) =
    Just $ SimpleDescr
        ((showSDoc . ppr) (unLoc name))
        (Just (BS.pack (showSDocUnqual $ ppr ts)))
        (Just (srcSpanToLocation loc))
        (toComment mbDoc [])
        True
extractMethod (_, mbDoc) = Nothing

extractConstructor decl@(L loc (ConDecl name _ _ _ _ _ doc)) =
    SimpleDescr
        ((showSDoc . ppr) (unLoc name))
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
            ((showSDoc . ppr) name)
            (Just (BS.pack (showSDocUnqual $ ppr typ)))
            (Just (srcSpanToLocation loc))
            (case doc of
                Nothing -> Nothing
                Just (L _ d) -> Just (BS.pack (printHsDoc d)))
            True
extractRecordFields _ = []

attachComments :: [LSig alpha] -> [LDocDecl alpha] -> [(LHsDecl alpha, Maybe (HsDoc alpha))]
attachComments sigs docs = collectDocs $ sortByLoc $
        ((map (\ (L l i) -> L l (SigD i)) sigs) ++ (map (\ (L l i) -> L l (DocD i)) docs))

sigToByteString ::  [(NSig, Maybe NDoc)] -> Maybe ByteString
sigToByteString [] = Nothing
sigToByteString [(sig,_)] = Just (BS.pack (showSDocUnqual $ppr sig))
sigToByteString ((sig,_):_) = Just (BS.pack (showSDocUnqual $ppr sig))

srcSpanToLocation :: SrcSpan -> Location
srcSpanToLocation span | not (isGoodSrcSpan span)
    =   error "srcSpanToLocation: unhelpful span"
srcSpanToLocation span
    =   Location (srcSpanStartLine span) (srcSpanStartCol span)
                 (srcSpanEndLine span) (srcSpanEndCol span)

toComment :: Outputable alpha => Maybe (HsDoc alpha) -> [HsDoc alpha] -> Maybe ByteString
toComment (Just c) _    =  Just (BS.pack (printHsDoc c))
toComment Nothing (c:_) =  Just (BS.pack (printHsDoc c))
toComment Nothing []    =  Nothing


{--
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [Data] []
collectParseInfoForDecl (l,st) ((Just (L loc (TyClD (TyFamily _ lid _ _)))), mbComment')
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [] []
collectParseInfoForDecl (l,st) ((Just (L loc (TyClD (ClassDecl _ lid _ _ _ _ _ _ )))), mbComment')
    =   addLocationAndComment (l,st) (unLoc lid) loc mbComment' [Class] []
--}

printHsDoc :: Outputable alpha => HsDoc alpha  -> String
printHsDoc d = show (PPDoc d)

-- Okay, I need to reconstruct the document comments, but for now:
--instance Outputable (DocDecl name) where
--  ppr _ = text "<**>"


newtype PPDoc alpha = PPDoc (HsDoc alpha)

instance Outputable alpha => Show (PPDoc alpha)  where
    showsPrec _ (PPDoc DocEmpty)                 =   id
    showsPrec _ (PPDoc (DocAppend l r))          =   shows (PPDoc l)  . shows (PPDoc r)
    showsPrec _ (PPDoc (DocString str))          =   showString str
    showsPrec _ (PPDoc (DocParagraph d))         =   shows (PPDoc d) . showChar '\n'
    showsPrec _ (PPDoc (DocIdentifier l))        =   foldr (\i f -> showChar '\'' .
                                                     ((showString . showSDoc .  ppr) i) . showChar '\'') id l
    showsPrec _ (PPDoc (DocModule str))          =   showChar '"' . showString str . showChar '"'
    showsPrec _ (PPDoc (DocEmphasis doc))        =   showChar '/' . shows (PPDoc doc)  . showChar '/'
    showsPrec _ (PPDoc (DocMonospaced doc))      =   showChar '@' . shows (PPDoc doc) . showChar '@'
    showsPrec _ (PPDoc (DocUnorderedList l))     =
        foldr (\s r -> showString "* " . shows (PPDoc s) . showChar '\n' . r) id l
    showsPrec _ (PPDoc (DocOrderedList l))       =
        foldr (\(i,n) f -> shows n . showSpace .  shows (PPDoc i)) id (zip l [1 .. length l])
    showsPrec _ (PPDoc (DocDefList li))          =
        foldr (\(l,r) f -> showString "[@" . shows (PPDoc l) . showString "[@ " . shows (PPDoc r) . f) id li
    showsPrec _ (PPDoc (DocCodeBlock doc))      =   showChar '@' . shows (PPDoc doc) . showChar '@'
    showsPrec _ (PPDoc (DocURL str))            =   showChar '<' . showString str . showChar '>'
    showsPrec _ (PPDoc (DocAName str))          =   showChar '#' . showString str . showChar '#'
    showsPrec _ (PPDoc _)                       =   id

---------------------------------------------------------------------------------
-- Now the interface file stuff

mayGetInterfaceFile :: PackageIdentifier -> ModuleName -> Ghc (Maybe (ModIface,FilePath))
mayGetInterfaceFile pid mn =
    let isBase  =   pkgName pid == (PackageName "base")
        mn'     =   mkModuleName (display mn)
        pid'    =   stringToPackageId (display pid)
        iface   =   findAndReadIface empty (if isBase
                                                then mkBaseModule_ mn'
                                                else mkModule pid' mn') False
        gblEnv  =   IfGblEnv { if_rec_types = Nothing }
    in do
        hscEnv              <-  getSession
        maybe               <-  liftIO $ initTcRnIf  'i' hscEnv gblEnv () iface
        case maybe of
            M.Succeeded val ->    return (Just val)
            _               ->    return Nothing

mayGetInterfaceDescription ::  PackageIdentifier -> ModuleName -> Ghc (Maybe ModuleDescr)
mayGetInterfaceDescription pid mn = do
    mbIf <- mayGetInterfaceFile pid mn
    case mbIf of
        Nothing -> trace ("no interface file for " ++ show mn) $ return Nothing
        Just (mif,_) ->
            let allDescrs  =    extractExportedDescrH pid mif
                mod        =    extractExportedDescrR pid allDescrs mif
            in trace ("interface file for " ++ show mn ++ " descrs: " ++ show (length (mdIdDescriptions mod)))
                $ return (Just mod)



