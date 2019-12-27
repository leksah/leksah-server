{-# LANGUAGE CPP, OverloadedStrings, TypeFamilies, FlexibleContexts #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.HeaderParser
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

module IDE.HeaderParser (

    parseTheHeader

) where

import Prelude ()
import Prelude.Compat hiding (readFile)
import IDE.Core.CTypes hiding(SrcSpan(..))
import GHC hiding (ImportDecl)
import FastString(unpackFS)
import IDE.Utils.GHCUtils
import Data.Maybe (mapMaybe)
#if MIN_VERSION_ghc(8,2,0)
import BasicTypes (StringLiteral(..), SourceText(..))
#elif MIN_VERSION_ghc(8,0,0)
import BasicTypes (StringLiteral(..))
#endif
import Outputable(pprPrefixOcc, ppr,showSDoc)
import IDE.Utils.FileUtils (figureOutHaddockOpts)
import Control.Monad.IO.Class (MonadIO(..))
import System.IO.Strict (readFile)
import qualified Data.Text as T (pack)
import System.Directory (setCurrentDirectory)
import System.FilePath (dropFileName)
import IDE.Utils.Project (ProjectKey)

#if !MIN_VERSION_ghc(8,2,0)
ieLWrappedName :: a -> a
ieLWrappedName = id
#endif

#if !MIN_VERSION_ghc(8,4,0)
type GhcPs = RdrName
type family IdP p
type instance IdP GhcPs = RdrName
#endif

#if MIN_VERSION_ghc(8,2,0)
unLoc82 ::
#if MIN_VERSION_ghc(8,8,0) && !MIN_VERSION_ghc(8,8,2)
  (HasSrcSpan (GenLocated l e)) =>
#endif
  GenLocated l e -> e
unLoc82 = unLoc
#else
unLoc82 :: a -> a
unLoc82 = id
#endif

showRdrName :: DynFlags -> RdrName -> String
showRdrName dflags r = showSDoc dflags (ppr r)

parseTheHeader :: ProjectKey -> FilePath -> FilePath -> IO ServerAnswer
parseTheHeader project package filePath = do
    setCurrentDirectory $ dropFileName package
    text        <- readFile filePath
    opts        <- figureOutHaddockOpts (Just project) package
    parseResult <- liftIO $ myParseHeader filePath text (filterOpts opts)
    case parseResult of
        Left str                                      -> return (ServerFailed str)
        Right (_, pr@HsModule{ hsmodImports = []})       -> do
            let i = case hsmodDecls pr of
                        decls@(_hd:_tl) -> foldl (\ a b -> min a (srcSpanStartLine' (getLoc b))) 0 decls - 1
                        [] -> case hsmodExports pr of
                            Just list -> foldl (\ a b -> max a (srcSpanEndLine' (getLoc b))) 0 (unLoc list) + 1
                            Nothing -> case hsmodName pr of
                                        Nothing -> 0
                                        Just mn -> srcSpanEndLine' (getLoc mn) + 2
            return (ServerHeader (Right i))
        Right (dflags, _pr@HsModule{ hsmodImports = imports }) -> return (ServerHeader (Left (transformImports dflags imports)))
  where
    filterOpts []    = []
    filterOpts (o:_:r) | o `elem` ["-link-js-lib", "-js-lib-outputdir", "-js-lib-src", "-package-id"] = filterOpts r
    filterOpts (o:r) = o:filterOpts r

transformImports :: DynFlags -> [LImportDecl GhcPs] -> [ImportDecl]
transformImports dflags = map (transformImport dflags)

transformImport :: DynFlags -> LImportDecl GhcPs -> ImportDecl
transformImport dflags (L srcSpan importDecl) =
    ImportDecl {
        importLoc = srcSpanToLocation srcSpan,
        importModule = T.pack modName,
        importQualified = ideclQualified importDecl,
        importSrc = ideclSource importDecl,
        importPkg = T.pack <$> pkgQual,
        importAs  = T.pack <$> impAs,
        importSpecs = specs}
    where
        modName =  moduleNameString $ unLoc $ ideclName importDecl
        pkgQual =  case ideclPkgQual importDecl of
#if MIN_VERSION_ghc(8,2,0)
                        Just StringLiteral { sl_st = SourceText s } -> Just s
#elif MIN_VERSION_ghc(8,0,0)
                        Just fs -> Just (sl_st fs)
#else
                        Just fs -> Just (unpackFS fs)
#endif
                        _ -> Nothing
        impAs   =  case ideclAs importDecl of
                        Nothing -> Nothing
                        Just mn -> Just (moduleNameString $ unLoc82 mn)
        specs =    case ideclHiding importDecl of
                        Nothing -> Nothing
                        Just (hide, list) -> Just (ImportSpecList hide (mapMaybe (transformEntity dflags) (unLoc list)))

transformEntity :: DynFlags -> LIE GhcPs -> Maybe ImportSpec
#if MIN_VERSION_ghc(8,6,0)
transformEntity dflags (L _ (IEVar _ name))              = Just (IVar (T.pack $ showSDoc dflags (pprPrefixOcc $ unLoc name)))
transformEntity dflags (L _ (IEThingAbs _ name))         = Just (IAbs (T.pack . showRdrName dflags . unLoc $ ieLWrappedName name))
transformEntity dflags (L _ (IEThingAll _ name))         = Just (IThingAll (T.pack . showRdrName dflags . unLoc $ ieLWrappedName name))
transformEntity dflags (L _ (IEThingWith _ name _ list _)) = Just (IThingWith (T.pack . showRdrName dflags . unLoc $ ieLWrappedName name)
#else
transformEntity dflags (L _ (IEVar name))              = Just (IVar (T.pack $ showSDoc dflags (pprPrefixOcc $ unLoc name)))
transformEntity dflags (L _ (IEThingAbs name))         = Just (IAbs (T.pack . showRdrName dflags . unLoc $ ieLWrappedName name))
transformEntity dflags (L _ (IEThingAll name))         = Just (IThingAll (T.pack . showRdrName dflags . unLoc $ ieLWrappedName name))
#if MIN_VERSION_ghc(8,0,0)
transformEntity dflags (L _ (IEThingWith name _ list _)) = Just (IThingWith (T.pack . showRdrName dflags . unLoc $ ieLWrappedName name)
#else
transformEntity dflags (L _ (IEThingWith name list))   = Just (IThingWith (T.pack . showRdrName dflags $ unLoc name)
#endif
#endif
                                                        (map (T.pack . showRdrName dflags . unLoc . ieLWrappedName) list))
transformEntity _ _                              = Nothing

srcSpanToLocation :: SrcSpan -> Location
srcSpanToLocation (RealSrcSpan span')
    =   Location (unpackFS $ srcSpanFile span') (srcSpanStartLine span') (srcSpanStartCol span')
                 (srcSpanEndLine span') (srcSpanEndCol span')
srcSpanToLocation _ = error "srcSpanToLocation: unhelpful span"

srcSpanStartLine' :: SrcSpan -> Int
srcSpanStartLine' (RealSrcSpan span') = srcSpanStartLine span'
srcSpanStartLine' _ = error "srcSpanStartLine': unhelpful span"

srcSpanEndLine' :: SrcSpan -> Int
srcSpanEndLine' (RealSrcSpan span') = srcSpanEndLine span'
srcSpanEndLine' _ = error "srcSpanEndLine': unhelpful span"
