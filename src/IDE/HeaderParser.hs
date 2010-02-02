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

parseTheHeader :: FilePath -> IO ServerAnswer
parseTheHeader filePath = do
    text        <- readFile filePath
    parseResult <- liftIO $ myParseHeader filePath text
    case parseResult of
        Left str                                      -> return (ServerFailed str)
        Right (pr@HsModule{ hsmodImports = imports }) -> return (ServerHeader (Just
                                                            (transformImports imports)))

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
transformEntity (L _ (IEVar name))              = Just (IVar (showRdrName name))
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

-- | A single Haskell @import@ declaration.
{--
type LImportDecl name = Located (ImportDecl name)
data ImportDecl name
  = ImportDecl {
      ideclName      :: Located ModuleName, -- ^ Module name.
      ideclPkgQual   :: Maybe FastString,   -- ^ Package qualifier.
      ideclSource    :: Bool,               -- ^ True <=> {-# SOURCE #-} import
      ideclQualified :: Bool,               -- ^ True => qualified
      ideclAs        :: Maybe ModuleName,   -- ^ as Module
      ideclHiding    :: Maybe (Bool, [LIE name]) -- ^ (True => hiding, names)
    }
type LIE name = Located (IE name)

-- | Imported or exported entity.
data IE name
  = IEVar               name
  | IEThingAbs          name		 -- ^ Class/Type (can't tell)
  | IEThingAll          name		 -- ^ Class/Type plus all methods/constructors
  | IEThingWith         name [name]	 -- ^ Class/Type plus some methods/constructors
  | IEModuleContents    ModuleName	 -- ^ (Export Only)
  | IEGroup             Int (HsDoc name) -- ^ Doc section heading
  | IEDoc               (HsDoc name)     -- ^ Some documentation
  | IEDocNamed          String           -- ^ Reference to named doc
--}
