-----------------------------------------------------------------------------
--
-- Module      :  IDE.HeaderParser
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

module IDE.HeaderParser (

    parseTheHeader

) where

import IDE.Core.CTypes hiding(SrcSpan(..))
import GHC hiding (ImportDecl)
import FastString(unpackFS)
import RdrName(showRdrName)
import IDE.Utils.GHCUtils
import Control.Monad.Trans (liftIO)
import Data.Maybe (mapMaybe)
import Outputable(pprHsVar,showSDoc)

parseTheHeader :: FilePath -> IO ServerAnswer
parseTheHeader filePath = do
    text        <- readFile filePath
    parseResult <- liftIO $ myParseHeader filePath text
    case parseResult of
        Left str                                      -> return (ServerFailed str)
        Right (pr@HsModule{ hsmodImports = []})       -> do
            let i = case hsmodDecls pr of
                        decls@(hd:tl) -> (foldl (\ a b -> min a (srcSpanStartLine (getLoc b))) 0 decls) - 1
                        [] -> case hsmodExports pr of
                            Just list ->  (foldl (\ a b -> max a (srcSpanEndLine (getLoc b))) 0 list) + 1
                            Nothing -> case hsmodName pr of
                                        Nothing -> 0
                                        Just mn -> srcSpanEndLine (getLoc mn) + 2
            return (ServerHeader (Right i))
        Right (pr@HsModule{ hsmodImports = imports }) -> return (ServerHeader (Left (transformImports imports)))

transformImports :: [LImportDecl RdrName] -> [ImportDecl]
transformImports = map transformImport

transformImport ::  LImportDecl RdrName -> ImportDecl
transformImport (L srcSpan importDecl) =
    ImportDecl {
        importLoc = srcSpanToLocation srcSpan,
        importModule = modName,
        importQualified = ideclQualified importDecl,
        importSrc = ideclSource importDecl,
        importPkg = pkgQual,
        importAs  = impAs,
        importSpecs = specs}
    where
        modName =  moduleNameString $ unLoc $ ideclName importDecl
        pkgQual =  case ideclPkgQual importDecl of
                        Nothing -> Nothing
                        Just fs -> Just (unpackFS fs)
        impAs   =  case ideclAs importDecl of
                        Nothing -> Nothing
                        Just mn -> Just (moduleNameString mn)
        specs =    case ideclHiding importDecl of
                        Nothing -> Nothing
                        Just (hide, list) -> Just (ImportSpecList hide (mapMaybe transformEntity list))

transformEntity :: LIE RdrName -> Maybe ImportSpec
transformEntity (L _ (IEVar name))              = Just (IVar (showSDoc (pprHsVar name)))
transformEntity (L _ (IEThingAbs name))         = Just (IAbs (showRdrName name))
transformEntity (L _ (IEThingAll name))         = Just (IThingAll (showRdrName name))	
transformEntity (L _ (IEThingWith name list))   = Just (IThingWith (showRdrName name)
                                                        (map showRdrName list))	
transformEntity  _                              = Nothing

srcSpanToLocation :: SrcSpan -> Location
srcSpanToLocation span | not (isGoodSrcSpan span)
    =   error "srcSpanToLocation: unhelpful span"
srcSpanToLocation span
    =   Location (srcSpanStartLine span) (srcSpanStartCol span)
                 (srcSpanEndLine span) (srcSpanEndCol span)
