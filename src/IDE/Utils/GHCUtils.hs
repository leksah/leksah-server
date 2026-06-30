{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Utils.GHCUtils
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  Jutaro <jutaro@leksah.org>
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
module IDE.Utils.GHCUtils (
    inGhcIO
,   getInstalledPackageInfos
,   unitInfoIdString
,   findFittingPackages
,   myParseModule
,   myParseHeader
,   mkDependency
,   viewDependency
,   LibraryName(..)
,   libraryNameToMaybe
,   maybeOrLibraryName
) where

import Prelude ()
import Prelude.Compat
import Distribution.Simple (withinRange,PackageIdentifier(..),Dependency(..), PackageName, VersionRange)
import Distribution.Text (simpleParse)
import Data.Maybe (fromJust)
import Distribution.Compat.NonEmptySet (NonEmptySet)
import Distribution.Compat.NonEmptySet as S (singleton)
import GHC
#if MIN_VERSION_ghc(9,0,0)
import GHC.Unit.Info (unitPackageIdString, unitId, UnitInfo)
import GHC.Unit.State (listUnitInfo)
import GHC.Unit.Types (unitIdString)
import GHC.Driver.Env (hsc_units)
import GHC.Driver.Pipeline(preprocess)
import GHC.Data.StringBuffer (StringBuffer(..),hGetStringBuffer)
import GHC.Data.FastString (mkFastString)
import GHC.Parser.Lexer (ParseResult,unP,initParserState,getPsMessages,pattern POk,pattern PFailed)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Config.Diagnostic (initPsMessageOpts)
import GHC.Types.Error (getMessages,errMsgDiagnostic,diagnosticMessage,unDecorated,Messages)
import GHC.Parser.Errors.Types (PsMessage)
import GHC.Utils.Outputable (ppr, vcat, renderWithContext, defaultSDocContext)
import GHC.Data.Bag (unitBag,bagToList)
import GHC.Utils.Error (errorsFound,showPass,)
import Control.Monad (unless, void)
import Data.Foldable (maximumBy)
import qualified GHC.Parser as P (parseModule,parseHeader)
import GHC.Hs.Stats (ppSourceStats)
import GHC.Types.SrcLoc (mkRealSrcLoc)
import GHC.Utils.Logger (getLogger)
#else
import PackageConfig (sourcePackageIdString, PackageConfig)
import DriverPipeline(preprocess)
import StringBuffer (StringBuffer(..),hGetStringBuffer)
import FastString (mkFastString)
import Lexer (mkPState,ParseResult(..),getMessages,unP)
import Outputable (ppr, showSDoc, vcat)
import Bag (unitBag,bagToList)
import ErrUtils (dumpIfSet_dyn,printBagOfErrors,errorsFound,mkPlainErrMsg,showPass,ErrMsg(..), ErrorMessages,pprErrMsgBagWithLoc)
import Control.Monad (unless, void)
import Data.Foldable (maximumBy)
import qualified Parser as P (parseModule,parseHeader)
import HscStats (ppSourceStats)
import SrcLoc (mkRealSrcLoc)
#endif
import IDE.Utils.FileUtils (getSysLibDir)
#if MIN_VERSION_ghc(9,0,0)
import GHC.Driver.Session (DumpFlag(..), gopt_set, PkgDbRef(..), PackageDBFlag(..))
#elif MIN_VERSION_ghc(8,2,0)
import DynFlags (DumpFlag(..), gopt_set, PkgConfRef(..), PackageDBFlag(..))
#else
import DynFlags (DumpFlag(..), gopt_set, PkgConfRef(..))
#endif
import System.Log.Logger(debugM)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.Function (on)
import GHC.Stack (HasCallStack)
import Distribution.Types.LibraryName (LibraryName(..))
import Distribution.Types.UnqualComponentName (UnqualComponentName)
#if MIN_VERSION_Cabal (3,4,0)
import Distribution.Types.Dependency (mkDependency)
#else
mkDependency :: PackageName -> VersionRange -> NonEmptySet LibraryName -> Dependency
mkDependency a b c = Dependency a b c
#endif

viewDependency :: Dependency -> (PackageName, VersionRange, NonEmptySet LibraryName)
viewDependency (Dependency a b c) = (a, b, c)
maybeOrLibraryName :: Maybe UnqualComponentName -> LibraryName
maybeOrLibraryName Nothing = LMainLibName
maybeOrLibraryName (Just n) = LSubLibName n
libraryNameToMaybe :: LibraryName -> Maybe UnqualComponentName
libraryNameToMaybe LMainLibName = Nothing
libraryNameToMaybe (LSubLibName n) = Just n

#if !MIN_VERSION_ghc(9,0,0)
type UnitInfo = PackageConfig
unitPackageIdString = sourcePackageIdString
#endif

inGhcIO :: HasCallStack => FilePath -> [Text] -> [GeneralFlag] -> [FilePath] -> (DynFlags -> Ghc a) -> IO a
inGhcIO libDir flags' udynFlags dbs ghcAct = do
    debugM "leksah-server" $ "inGhcIO called with: " ++ show flags'
--    (restFlags, _) <-   parseStaticFlags (map noLoc flags')
    runGhc (Just libDir) $ do
        dynflags  <- getSessionDynFlags
        let dynflags' = foldl gopt_set dynflags udynFlags
        let dynflags'' = dynflags' {
#if !MIN_VERSION_ghc(9,0,0)
            hscTarget = HscNothing,
#endif
            ghcMode   = CompManager,
            ghcLink   = NoLink,
#if MIN_VERSION_ghc(9,0,0)
            packageDBFlags = map (PackageDB . PkgDbPath) dbs ++ packageDBFlags dynflags'
#elif MIN_VERSION_ghc(8,2,0)
            packageDBFlags = map (PackageDB . PkgConfFile) dbs ++ packageDBFlags dynflags'
#else
            extraPkgConfs = (map PkgConfFile dbs++) . extraPkgConfs dynflags'
#endif
            }
        dynflags''' <- parseGhcFlags dynflags'' (map (noLoc . T.unpack) flags') flags'
        _ <- setSessionDynFlags dynflags'''
        res <- getSessionDynFlags >>= ghcAct
        unload
        return res
    where
        parseGhcFlags :: DynFlags -> [Located String]
                  -> [Text] -> Ghc DynFlags
        parseGhcFlags dynflags flags_ _origFlags = do
            logger <- getLogger
            (dynflags', rest, _) <- parseDynamicFlags logger dynflags flags_
            if not (null rest)
                then do
                    liftIO $ debugM "leksah-server" ("No dynamic GHC options: " ++ unwords (map unLoc rest))
                    return dynflags'
                else return dynflags'

-- | Unload whatever is currently loaded.
unload :: Ghc ()
unload = do
   setTargets []
   void $ load LoadAllTargets

getInstalledPackageInfos :: Ghc [UnitInfo]
getInstalledPackageInfos = do
#if MIN_VERSION_ghc(9,0,0)
    -- The unit state holds every package known to the session (the package
    -- dbs passed to `inGhcIO` are folded in by `setSessionDynFlags`).
    -- `listUnitInfo` enumerates them all, hidden ones included.
    hscEnv <- getSession
    return . listUnitInfo $ hsc_units hscEnv
#else
    dflags1         <-  getSessionDynFlags
    case pkgDatabase dflags1 of
        Nothing -> return []
#if MIN_VERSION_ghc(8,0,0)
        Just fm -> return (fm >>= snd)
#else
        Just fm -> return fm
#endif
#endif

-- | The full installed unit id of a package (name-version-hash), in the same
-- form cabal writes to plan.json's @id@ field — so the collector can match a
-- package against the project's install plan exactly (not just by name-version,
-- which would be ambiguous when the store holds several builds of a version).
unitInfoIdString :: UnitInfo -> String
#if MIN_VERSION_ghc(9,0,0)
unitInfoIdString = unitIdString . unitId
#else
unitInfoIdString = unitPackageIdString
#endif

findFittingPackages :: [Dependency] -> Ghc [PackageIdentifier]
findFittingPackages dependencyList = do
    knownPackages   <-  getInstalledPackageInfos
    let packages    =   map (fromJust . simpleParse . unitPackageIdString) knownPackages
    return (concatMap (fittingKnown packages) dependencyList)
    where
    fittingKnown packages (viewDependency -> (dname, versionRange, _)) =
        let filtered =  filter (\ (PackageIdentifier name version) ->
                                    name == dname && withinRange version versionRange)
                        packages
        in  if length filtered > 1
                then [maximumBy (compare `on` pkgVersion) filtered]
                else filtered

 ---------------------------------------------------------------------
--  | Parser function copied here, because it is not exported

#if MIN_VERSION_ghc(9,0,0)
myParseModule :: DynFlags -> FilePath -> Maybe StringBuffer -> IO (Either (Messages PsMessage) (Located (HsModule GhcPs)))
myParseModule dflags src_filename maybe_src_buf = do
  buf' <- case maybe_src_buf of
            Just b  -> return b
            Nothing -> hGetStringBuffer src_filename
  let loc = mkRealSrcLoc (mkFastString src_filename) 1 0
      pst = initParserState (initParserOpts dflags) buf' loc
  case unP P.parseModule pst of
    PFailed pst'      -> return (Left (snd (getPsMessages pst')))
    POk _ rdr_module  -> return (Right rdr_module)
myParseHeader :: FilePath -> String -> [Text] -> IO (Either Text (DynFlags, HsModule GhcPs))
myParseHeader fp _str opts =
  getSysLibDir Nothing (Just VERSION_ghc) >>= \case
    Nothing -> return . Left $ "myParseHeader could not find system lib dir for GHC " <> VERSION_ghc <> " (used to build Leksah)"
    Just libDir ->
      inGhcIO libDir (opts++["-cpp"]) [] [] $ \ _dynFlags -> do
        session <- getSession
        liftIO $
          preprocess session fp Nothing Nothing >>= \case
            Left _errs ->
                return . Left $ "Failed to preprocess " <> T.pack fp
            Right (dynFlags', fp') -> do
                buf <- hGetStringBuffer fp'
                let loc = mkRealSrcLoc (mkFastString fp) 1 0
                    pst = initParserState (initParserOpts dynFlags') buf loc
                case unP P.parseHeader pst of
                    POk _ located -> return (Right (dynFlags', unLoc located))
                    PFailed pst' ->
                        let (_warns, errs) = getPsMessages pst'
                            msgOpts = initPsMessageOpts dynFlags'
                            doc = vcat
                                [ d
                                | e <- bagToList (getMessages errs)
                                , d <- unDecorated (diagnosticMessage msgOpts (errMsgDiagnostic e))
                                ]
                        in return . Left $
                            "Failed to parse " <> T.pack fp <> ":\n"
                            <> T.pack (renderWithContext defaultSDocContext doc)
#else
myParseModule :: DynFlags -> FilePath -> Maybe StringBuffer
              -> IO (Either ErrorMessages (Located (HsModule
#if MIN_VERSION_ghc(8,4,0)
                      GhcPs
#else
                      RdrName
#endif
                      )))
myParseModule dflags src_filename maybe_src_buf
 =    --------------------------  Parser  ----------------
      showPass dflags "Parser" >>
      {-# SCC "Parser" #-} do

        -- sometimes we already have the buffer in memory, perhaps
        -- because we needed to parse the imports out of it, or get the
        -- module name.
      buf' <- case maybe_src_buf of
                Just b  -> return b
                Nothing -> hGetStringBuffer src_filename

      let loc  = mkRealSrcLoc (mkFastString src_filename) 1 0

      case unP P.parseModule (mkPState dflags buf' loc) of
        PFailed
#if MIN_VERSION_ghc(8,10,0)
          pst ->
            return (Left (snd (getMessages pst dflags)))
#else
          _ span' err -> return (Left (unitBag (mkPlainErrMsg dflags span' err)))
#endif
        POk pst rdr_module -> do
            let ms@(warnings, errors) = getMessages pst dflags
            printBagOfErrors dflags errors;
            unless (errorsFound dflags ms) $ printBagOfErrors dflags warnings;
            dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" $
                                   ppr rdr_module
            dumpIfSet_dyn dflags Opt_D_source_stats "Source Statistics" $
                                   ppSourceStats False rdr_module
            return (Right rdr_module)

myParseHeader :: FilePath -> String -> [Text] -> IO (Either Text (DynFlags, HsModule
#if MIN_VERSION_ghc(8,4,0)
                      GhcPs
#else
                      RdrName
#endif
                      ))
myParseHeader fp _str opts =
  getSysLibDir Nothing (Just VERSION_ghc) >>= \case
    Nothing -> return . Left $ "myParseHeader could not find system lib dir for GHC " <> VERSION_ghc <> " (used to build Leksah)"
    Just libDir ->
      inGhcIO libDir (opts++["-cpp"]) [] [] $ \ _dynFlags -> do
        session   <- getSession
#if MIN_VERSION_ghc(8,8,0)
        liftIO $
          preprocess session fp Nothing Nothing >>= \case
            Left errMsg -> do
                let str =  "Failed to preprocess " <> T.pack fp
                return (Left str)
            Right (dynFlags',fp') -> do
#else
        liftIO $ do
                (dynFlags',fp') <- preprocess session (fp,Nothing)
#endif
                stringBuffer  <-  hGetStringBuffer fp'
                parseResult   <-  myParseModuleHeader dynFlags' fp (Just stringBuffer)
                case parseResult of
                    Right (L _ mod') -> return (Right (dynFlags', mod'))
                    Left errMsg         -> do
                        let str =  "Failed to parse " <> T.pack (showSDoc dynFlags' $ vcat $ pprErrMsgBagWithLoc errMsg)
                        return (Left str)

 ---------------------------------------------------------------------
--  | Parser function copied here, because it is not exported

myParseModuleHeader :: DynFlags -> FilePath -> Maybe StringBuffer
              -> IO (Either ErrorMessages (Located (HsModule
#if MIN_VERSION_ghc(8,4,0)
                      GhcPs
#else
                      RdrName
#endif
                      )))
myParseModuleHeader dflags src_filename maybe_src_buf
 =  --------------------------  Parser  ----------------
    showPass dflags "Parser" >>
    {-# SCC "Parser" #-} do

        -- sometimes we already have the buffer in memory, perhaps
        -- because we needed to parse the imports out of it, or get the
        -- module name.
      buf' <- case maybe_src_buf of
                Just b  -> return b
                Nothing -> hGetStringBuffer src_filename

      let loc  = mkRealSrcLoc (mkFastString src_filename) 1 0

      case unP P.parseHeader (mkPState dflags buf' loc) of
        PFailed
#if MIN_VERSION_ghc(8,10,0)
          pst ->
            return (Left (snd (getMessages pst dflags)))
#else
          _ span' err -> return (Left (unitBag (mkPlainErrMsg dflags span' err)))
#endif
        POk pst rdr_module -> do
            let ms@(warnings, errors) = getMessages pst dflags
            printBagOfErrors dflags errors;
            unless (errorsFound dflags ms) $ printBagOfErrors dflags warnings;
            dumpIfSet_dyn dflags Opt_D_dump_parsed "Parser" $
                                   ppr rdr_module
            dumpIfSet_dyn dflags Opt_D_source_stats "Source Statistics" $
                                   ppSourceStats False rdr_module
            return (Right rdr_module)
#endif
