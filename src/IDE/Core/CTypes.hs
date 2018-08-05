{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Core.CTypes
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

module IDE.Core.CTypes (

    PackageDescr(..)
,   ModuleKey(..)
,   displayModuleKey
,   parseModuleKey
,   moduleKeyToName
,   ModuleDescr(..)
,   Descr(..)
,   RealDescr(..)
,   ReexportedDescr(..)
,   Present(..)
,   TypeDescr(..)
,   DescrType(..)
,   SimpleDescr(..)
,   GenScope(..)
,   dscName
,   dscMbTypeStr
,   dscMbModu
,   dsMbModu
,   dscMbLocation
,   dscMbComment
,   dscTypeHint
,   dscExported
,   descrType
,   isReexported
,   PackScope(..)
,   SymbolTable(..)
,   PackModule(..)
,   parsePackModule
,   showPackModule
,   packageIdentifierToString
,   packageIdentifierFromString
,   Location(..)
,   SrcSpan(..)
,   Scope(..)

,   ServerCommand(..)
,   ServerAnswer(..)

,   leksahVersion
,   configDirName
,   metadataVersion

,   ImportDecl(..)
,   ImportSpecList(..)
,   ImportSpec(..)

,   getThisPackage
,   PackageIdAndKey(..)
,   RetrieveStrategy(..)

,   prettyPrint
) where

import Prelude ()
import Prelude.Compat
import Data.Typeable (Typeable)
import Data.Map (Map)
import Data.Set (Set)
import Data.Maybe (fromMaybe)
import Data.Default (Default(..))
import Distribution.Package
       (PackageIdentifier(..))
import Distribution.ModuleName (main, components, ModuleName)
import Data.ByteString.Char8 (ByteString)
#if !MIN_VERSION_bytestring(0,10,0)
import Data.Version (Version(..))
#endif
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))
import Distribution.Text (simpleParse, display)
import qualified Data.ByteString.Char8 as BS (unpack, empty)
import qualified Data.Map as Map (lookup,keysSet,splitLookup, insertWith,empty,elems,union)
import Data.Char (isAlpha, isSpace)
import Control.DeepSeq (NFData(..))
import PackageConfig (PackageConfig)
#if MIN_VERSION_ghc(8,0,0)
#if MIN_VERSION_ghc(8,2,0)
import Module (InstalledUnitId)
#else
import Module (UnitId)
#endif
import PackageConfig (sourcePackageIdString, unitId)
import Data.Maybe (fromJust)
#else
import Module (PackageKey)
import PackageConfig (sourcePackageIdString, packageKey)
import Data.Maybe (fromJust)
#endif
import Data.Text (Text)
import Text.PrettyPrint (fsep, Doc, (<+>), empty, text)
import qualified Text.PrettyPrint as PP
       (text, comma, punctuate, parens, Doc, empty, renderStyle, style)
import qualified Data.Text as T (pack, tail, span, unpack)

-- ---------------------------------------------------------------------
--  | Information about the system, extraced from .hi and source files
--

leksahVersion, configDirName :: FilePath
leksahVersion = "0.17"
configDirName = ".leksah-" <> leksahVersion

metadataVersion :: Integer
metadataVersion = 7

data PackageIdAndKey = PackageIdAndKey {
      packId  :: PackageIdentifier
#if MIN_VERSION_ghc(8,2,0)
    , packUnitId :: InstalledUnitId
#elif MIN_VERSION_ghc(8,0,0)
    , packUnitId :: UnitId
#else
    , packKey :: PackageKey
#endif
    }

getThisPackage :: PackageConfig -> PackageIdAndKey
getThisPackage p = PackageIdAndKey
#if MIN_VERSION_ghc(8,0,0)
                        (fromJust . simpleParse $ sourcePackageIdString p)
                        (unitId p)
#else
                        (fromJust . simpleParse $ sourcePackageIdString p)
                        (packageKey p)
#endif

data RetrieveStrategy = RetrieveThenBuild | BuildThenRetrieve | NeverRetrieve
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON RetrieveStrategy
instance FromJSON RetrieveStrategy

data ServerCommand =
        SystemCommand {
            scRebuild    :: Bool,
            scSources    :: Bool,
            scExtract    :: Bool,
            scPackageDBs :: [(Maybe FilePath, [FilePath])]}
    |   WorkspaceCommand {
            wcRebuild     :: Bool,
            wcPackage     :: PackageIdentifier,
            wcProject     :: FilePath,
            wcPackageFile :: FilePath,
            wcModList     :: [(Text,FilePath)]}
    |   ParseHeaderCommand {
            hcProject     :: FilePath,
            hcPackageFile :: FilePath,
            hcFilePath    :: FilePath}
    deriving (Eq,Ord,Show,Read)

data ServerAnswer = ServerOK
    | ServerFailed Text
    | ServerHeader (Either [ImportDecl] Int)
    deriving (Eq,Ord,Show,Read)


data PackScope  alpha       =   SymbolTable alpha => PackScope (Map PackageIdentifier PackageDescr) alpha
data GenScope           =   forall alpha. SymbolTable alpha  => GenScopeC (PackScope alpha)

class SymbolTable alpha  where
    symLookup       :: Text  -> alpha -> [Descr]
    symbols         :: alpha -> Set Text
    symSplitLookup  :: Text  -> alpha -> (alpha , Maybe [Descr], alpha)
    symInsert       :: Text  -> [Descr] -> alpha -> alpha
    symEmpty        :: alpha
    symElems        :: alpha -> [[Descr]]
    symUnion        :: alpha -> alpha -> alpha

instance SymbolTable (Map Text [Descr]) where
    symLookup str smap  = fromMaybe [] (str `Map.lookup` smap)
    symbols             = Map.keysSet
    symSplitLookup      = Map.splitLookup
    symInsert           = Map.insertWith (++)
    symEmpty            = Map.empty
    symElems            = Map.elems
    symUnion            = Map.union

data PackageDescr       =   PackageDescr {
        pdPackage           ::   PackageIdentifier
    ,   pdMbSourcePath      ::   Maybe FilePath
    ,   pdModules           ::   [ModuleDescr]
    ,   pdBuildDepends      ::   [PackageIdentifier]
} deriving (Show,Typeable)

instance Default PackageDescr where
    def = PackageDescr def def def def

newtype Present alpha       =   Present alpha

instance Show (Present PackageDescr) where
    show (Present pd)   =   T.unpack $ (packageIdentifierToString . pdPackage) pd

instance Eq PackageDescr where
    (== ) a b            =   pdPackage a == pdPackage b

instance Ord PackageDescr where
    (<=) a b              =   pdPackage a <=  pdPackage b

-- | Key we use to identify a module in the metadata.
-- But for "Main" modules we need to know the source file path.
data ModuleKey = LibModule ModuleName | MainModule FilePath deriving (Show, Eq, Ord)

-- | Display a the module key
displayModuleKey :: ModuleKey -> String
displayModuleKey (LibModule m) = display m
displayModuleKey (MainModule f) = "Main (" ++ f ++ ")"

-- | Parse a module key from the name of the module and the source file path.
parseModuleKey :: String -> FilePath -> Maybe ModuleKey
parseModuleKey "Main" sourcePath = Just $ MainModule sourcePath
parseModuleKey s _ = LibModule <$> simpleParse s

-- | Extract the module name from the module key
moduleKeyToName :: ModuleKey -> ModuleName
moduleKeyToName (LibModule m) = m
moduleKeyToName (MainModule _) = main

data ModuleDescr        =   ModuleDescr {
        mdModuleId          ::   PackModule
    ,   mdMbSourcePath      ::   Maybe FilePath                  -- unqualified
    ,   mdReferences        ::   Map ModuleName (Set Text) -- imports
    ,   mdIdDescriptions    ::   [Descr]
} deriving (Show,Typeable)

instance Default ModuleDescr where
    def = ModuleDescr def def Map.empty def

instance Show (Present ModuleDescr) where
    show (Present md)   =   (show . mdModuleId) md

instance Eq ModuleDescr where
    (== ) a b            =   let
        idEq = mdModuleId a == mdModuleId b
        -- Main modules are only the same if they use the same file
        in if idEq && ["Main"]  == components (modu (mdModuleId a))
                then mdMbSourcePath a == mdMbSourcePath b
                else idEq

instance Ord ModuleDescr where
    (<=) a b             =   let
        idEq = mdModuleId a == mdModuleId b
        -- use source files for main modules
        in if idEq && ["Main"]  == components (modu (mdModuleId a))
                then mdMbSourcePath a <= mdMbSourcePath b
                else mdModuleId a <=  mdModuleId b

data Descr =  Real RealDescr | Reexported ReexportedDescr
        deriving (Show,Read,Typeable,Eq,Ord,Generic)

instance ToJSON Descr
instance FromJSON Descr

data RealDescr          =   RealDescr {
        dscName'        ::   Text
    ,   dscMbTypeStr'   ::   Maybe ByteString
    ,   dscMbModu'      ::   Maybe PackModule
    ,   dscMbLocation'  ::   Maybe Location
    ,   dscMbComment'   ::   Maybe ByteString
    ,   dscTypeHint'    ::   TypeDescr
    ,   dscExported'    ::   Bool
    }
        deriving (Show,Read,Typeable,Generic)

instance ToJSON RealDescr where
    toJSON = toJSON . show
instance FromJSON RealDescr where
    parseJSON v = read <$> parseJSON v

data ReexportedDescr    =   ReexportedDescr {
        dsrMbModu       ::   Maybe PackModule
    ,   dsrDescr        ::   Descr}
        deriving (Show,Read,Typeable,Generic)

instance ToJSON ReexportedDescr where
    toJSON = toJSON . show
instance FromJSON ReexportedDescr where
    parseJSON v = read <$> parseJSON v

-- Metadata accessors

isReexported :: Descr -> Bool
isReexported (Reexported _)     =   True
isReexported _                  =   False

dscName :: Descr -> Text
dscName (Reexported d)          = dscName (dsrDescr d)
dscName (Real d)                = dscName' d

dscMbTypeStr :: Descr -> Maybe ByteString
dscMbTypeStr (Reexported d)     = dscMbTypeStr (dsrDescr d)
dscMbTypeStr (Real d)           = dscMbTypeStr' d

-- | The definition module
dscMbModu :: Descr -> Maybe PackModule
dscMbModu (Reexported d)        = dscMbModu (dsrDescr d)
dscMbModu (Real d)              = dscMbModu' d

-- | The exporting module
dsMbModu :: Descr -> Maybe PackModule
dsMbModu (Reexported d)        = dsrMbModu d
dsMbModu (Real d)              = dscMbModu' d

dscMbLocation :: Descr -> Maybe Location
dscMbLocation (Reexported d)    = dscMbLocation (dsrDescr d)
dscMbLocation (Real d)          = dscMbLocation' d

dscMbComment :: Descr -> Maybe ByteString
dscMbComment (Reexported d)     = dscMbComment (dsrDescr d)
dscMbComment (Real d)           = dscMbComment' d

dscTypeHint :: Descr -> TypeDescr
dscTypeHint (Reexported d)      = dscTypeHint (dsrDescr d)
dscTypeHint (Real d)            = dscTypeHint' d

dscExported :: Descr -> Bool
dscExported (Reexported _)      = True
dscExported (Real d)            = dscExported' d

data TypeDescr   =
        VariableDescr
    |   FieldDescr Descr
    |   ConstructorDescr Descr
    |   DataDescr [SimpleDescr] [SimpleDescr] -- ^ first constructors, then fields
    |   TypeDescr
    |   NewtypeDescr SimpleDescr (Maybe SimpleDescr) -- ^ first constructors, then maybe field
    |   ClassDescr  [Text] [SimpleDescr] -- ^ first super, then methods
    |   MethodDescr Descr -- ^ classDescr
    |   InstanceDescr [Text] -- ^ binds
    |   KeywordDescr
    |   ExtensionDescr
    |   ModNameDescr
    |   QualModNameDescr
    |   ErrorDescr
    |   PatternSynonymDescr
            --the descrName is the type Konstructor?
        deriving (Show,Read,Eq,Ord,Typeable)

data DescrType = Variable | Field | Constructor | Data  | Type | Newtype
    | Class | Method | Instance | Keyword | Extension | ModName | QualModName | Error | PatternSynonym
  deriving (Show, Eq, Ord, Bounded, Enum, Read)

instance Default DescrType where
    def = Variable

data SimpleDescr = SimpleDescr {
    sdName      :: Text,
    sdType      :: Maybe ByteString,
    sdLocation  :: Maybe Location,
    sdComment   :: Maybe ByteString,
    sdExported  :: Bool}
        deriving (Show,Read,Eq,Ord,Typeable)

descrType ::  TypeDescr -> DescrType
descrType VariableDescr      =   Variable
descrType (FieldDescr _)     =   Field
descrType (ConstructorDescr _) = Constructor
descrType (DataDescr _ _)    =   Data
descrType TypeDescr          =   Type
descrType (NewtypeDescr _ _) =   Newtype
descrType (ClassDescr  _ _)  =   Class
descrType (MethodDescr _)    =   Method
descrType (InstanceDescr _)  =   Instance
descrType KeywordDescr       =   Keyword
descrType ExtensionDescr     =   Extension
descrType ModNameDescr       =   ModName
descrType QualModNameDescr   =   QualModName
descrType ErrorDescr         =   Error
descrType PatternSynonymDescr =  PatternSynonym

data PackModule         =   PM {    pack :: PackageIdentifier
                                ,   modu :: ModuleName}
                                deriving (Eq, Ord,Read,Show,Typeable)

instance Show (Present PackModule) where
    showsPrec _ (Present pd)  =   showString (T.unpack $ (packageIdentifierToString . pack) pd) . showChar ':'
                                    .  showString (display (modu pd))

parsePackModule         ::   Text -> PackModule
parsePackModule str     =   let (pack',mod') = T.span (/= ':') str
                            in case packageIdentifierFromString pack' of
                                Nothing -> perror . T.unpack $ "Types>>parsePackModule: Can't parse package:" <> str
                                Just pi'-> case simpleParse . T.unpack $ T.tail mod' of
                                            Nothing -> perror . T.unpack $
                                                "Types>>parsePackModule: Can't parse module:" <> str
                                            Just mn -> PM pi' mn
    where perror s      =   error $ "cannot parse PackModule from " ++ s

showPackModule :: PackModule -> Text
showPackModule              = T.pack . show . Present

packageIdentifierToString :: PackageIdentifier -> Text
packageIdentifierToString   = T.pack . display

packageIdentifierFromString :: Text -> Maybe PackageIdentifier
packageIdentifierFromString = simpleParse . T.unpack

nonEmptyLines :: String -> [String]
nonEmptyLines = filter (any (not . isSpace)) . lines

instance Show (Present Descr) where
    showsPrec _ (Present descr) =   case dscMbComment descr of
                                        Just comment -> p . showChar '\n' . c comment . t
                                        Nothing      -> p . showChar '\n' . showChar '\n' . t
        where p         =   case dsMbModu descr of
                                Just ds -> showString "-- " . shows (Present ds)
                                Nothing -> id
              c com     =   showString $ unlines
                                $ map (\(i,l) -> if i == 0 then "-- | " ++ l else "--  " ++ l)
                                    $ zip [0 .. length nelines - 1] nelines
                                where nelines = nonEmptyLines $ BS.unpack com
              t         =   case dscMbTypeStr descr of
                                Just ti -> showString $ BS.unpack ti
                                Nothing -> id

instance Eq RealDescr where
    (== ) a b             =   dscName' a == dscName' b
                                && dscTypeHint' a   == dscTypeHint' b

instance Ord RealDescr where
    (<=) a b             =   if dscName' a == dscName' b
                                then dscTypeHint' a   <= dscTypeHint' b
                                else dscName' a <  dscName' b

instance Eq ReexportedDescr where
    (== ) a b             =   dscName (Reexported a) == dscName (Reexported b)
                                && dscTypeHint (Reexported a)   == dscTypeHint (Reexported b)

instance Ord ReexportedDescr where
    (<=) a b             =   if dscName (Reexported a) == dscName (Reexported b)
                                then dscTypeHint (Reexported a)   <= dscTypeHint (Reexported b)
                                else dscName (Reexported a) <  dscName (Reexported b)

instance Default PackModule where
    def = parsePackModule "unknow-0:Undefined"

instance Default PackageIdentifier where
    def = fromMaybe
                   (error "CTypes.getDefault: Can't parse Package Identifier")
                   (packageIdentifierFromString "unknown-0")

-- | A portion of the source, spanning one or more lines and zero or more columns.
data SrcSpan = SrcSpan
    { srcSpanFilename    :: FilePath
    , srcSpanStartLine   :: Int
    , srcSpanStartColumn :: Int
    , srcSpanEndLine     :: Int
    , srcSpanEndColumn   :: Int
    }
  deriving (Eq,Ord,Show)

data Location           =   Location {
    locationFile        ::   FilePath
,   locationSLine       ::   Int
,   locationSCol        ::   Int
,   locationELine       ::   Int
,   locationECol        ::   Int
}   deriving (Show,Eq,Ord,Read,Typeable)

instance Default ByteString
    where def = BS.empty

data Scope = PackageScope Bool | WorkspaceScope Bool | SystemScope
    -- True -> with imports, False -> without imports
  deriving (Show, Eq, Read, Generic)

instance ToJSON Scope
instance FromJSON Scope

instance Ord Scope where
    _ <= SystemScope                             = True
    WorkspaceScope False <=  WorkspaceScope True = True
    WorkspaceScope False <=  PackageScope True   = True
    PackageScope True    <=  WorkspaceScope True = True
    PackageScope False   <=  PackageScope True   = True
    _ <= _  = False

-- | An import declaration.
data ImportDecl = ImportDecl
    { importLoc :: Location
    , importModule :: Text   -- ^ name of the module imported.
    , importQualified :: Bool          -- ^ imported @qualified@?
    , importSrc :: Bool                -- ^ imported with @{-\# SOURCE \#-}@?
    , importPkg :: Maybe Text        -- ^ imported with explicit package name
    , importAs :: Maybe Text -- ^ optional alias name in an @as@ clause.
    , importSpecs :: Maybe ImportSpecList
            -- ^ optional list of import specifications.
    }
  deriving (Eq,Ord,Read,Show)

-- ------------------------------------------------------------
-- * Printing
-- ------------------------------------------------------------
-- | pretty-print with the default style and 'defaultMode'.
prettyPrint :: Pretty a => a -> Text
prettyPrint a = T.pack $ PP.renderStyle PP.style (pretty a)

-- | Things that can be pretty-printed
class Pretty a where
    -- | Pretty-print something in isolation.
    pretty :: a -> PP.Doc
    -- | Pretty-print something in a precedence context.
    prettyPrec :: Int -> a -> PP.Doc
    pretty = prettyPrec 0
    prettyPrec _ = pretty

emptyPrinter ::  () ->  PP.Doc
emptyPrinter _ = PP.empty

maybePP :: (a -> PP.Doc) -> Maybe a -> PP.Doc
maybePP _ Nothing = PP.empty
maybePP pp (Just a) = pp a

instance Pretty Text where
    pretty str = PP.text $ T.unpack str

instance Pretty ImportDecl
  where
    pretty (ImportDecl _ mod' qual _ _ mbName mbSpecs) =
        mySep [text "import",
               if qual then text "qualified" else empty,
               pretty mod',
               maybePP (\m' -> text "as" <+> pretty m') mbName,
               maybePP exports mbSpecs]
      where
        exports (ImportSpecList b specList) =
            if b then text "hiding" <+> specs else specs
                where specs = parenList . map pretty $ specList

parenList :: [Doc] -> Doc
parenList = PP.parens . fsep . PP.punctuate PP.comma

mySep :: [Doc] -> Doc
mySep [x]    = x
mySep (x:xs) = x <+> fsep xs
mySep []     = error "Internal error: mySep"

-- | An explicit import specification list.
data ImportSpecList
    = ImportSpecList Bool [ImportSpec]
            -- A list of import specifications.
            -- The 'Bool' is 'True' if the names are excluded
            -- by @hiding@.
  deriving (Eq,Ord,Read,Show)

-- | An import specification, representing a single explicit item imported
--   (or hidden) from a module.
data ImportSpec
     = IVar Text                  -- ^ variable
     | IAbs Text                 -- ^ @T@:
                                        --   the name of a class, datatype or type synonym.
     | IThingAll Text             -- ^ @T(..)@:
                                        --   a class imported with all of its methods, or
                                        --   a datatype imported with all of its constructors.
     | IThingWith Text [Text]  -- ^ @T(C_1,...,C_n)@:
                                        --   a class imported with some of its methods, or
                                        --   a datatype imported with some of its constructors.
  deriving (Eq,Ord,Read,Show)

newtype VName = VName Text

instance Pretty ImportSpec where
    pretty (IVar name)                = pretty (VName name)
    pretty (IAbs name)                = pretty name
    pretty (IThingAll name)           = pretty name <> text "(..)"
    pretty (IThingWith name nameList) =
        pretty name <> parenList (map (pretty . VName) nameList)

instance Pretty VName  where
    pretty (VName t) = let str = T.unpack t in if isOperator str then PP.parens (PP.text str) else PP.text str

isOperator :: String -> Bool
isOperator ('(':_)   =  False              -- (), (,) etc
isOperator ('[':_)   =  False              -- []
isOperator ('$':c:_) =  not (isAlpha c)    -- Don't treat $d as an operator
isOperator (':':c:_) =  not (isAlpha c)    -- Don't treat :T as an operator
isOperator ('_':_)   =  False              -- Not an operator
isOperator (c:_)     =  not (isAlpha c)    -- Starts with non-alpha
isOperator _         =  False

-- ---------------------------------------------------------------------
-- NFData instances for forcing evaluation
--
#if MIN_VERSION_deepseq(1,2,0) && !MIN_VERSION_containers(0,4,2)
instance (NFData k, NFData a) => NFData (Map k a) where
    rnf = rnf . Map.toList

instance NFData a => NFData (Set a) where
    rnf = rnf . Set.toList
#endif

instance NFData Location where

    rnf pd =  rnf (locationSLine pd)
                    `seq`    rnf (locationSCol pd)
                    `seq`    rnf (locationELine pd)
                    `seq`    rnf (locationECol pd)

instance NFData PackageDescr where
    rnf pd =  rnf (pdPackage pd)
                    `seq`    rnf (pdMbSourcePath pd)
                    `seq`    rnf (pdModules pd)
                    `seq`    rnf (pdBuildDepends pd)

instance NFData ModuleDescr where
    rnf pd =  rnf (mdModuleId pd)
                    `seq`    rnf (mdMbSourcePath pd)
                    `seq`    rnf (mdReferences pd)
                    `seq`    rnf (mdIdDescriptions pd)

instance NFData Descr where
    rnf (Real (RealDescr dscName'' dscMbTypeStr'' dscMbModu''
        dscMbLocation'' dscMbComment'' dscTypeHint'' dscExported''))  =  rnf dscName''
                    `seq`    rnf dscMbTypeStr''
                    `seq`    rnf dscMbModu''
                    `seq`    rnf dscMbLocation''
                    `seq`    rnf dscMbComment''
                    `seq`    rnf dscTypeHint''
                    `seq`    rnf dscExported''

    rnf (Reexported (ReexportedDescr reexpModu' impDescr')) = rnf reexpModu'
                    `seq`    rnf impDescr'

instance NFData TypeDescr where
    rnf (FieldDescr typeDescrF')              =   rnf typeDescrF'
    rnf (ConstructorDescr typeDescrC')        =   rnf typeDescrC'
    rnf (DataDescr constructors' fields')     =   constructors'
                    `seq` rnf fields'
    rnf (NewtypeDescr constructor' mbField')  =   rnf constructor'
                    `seq`    rnf mbField'
    rnf (ClassDescr super' methods')          =   rnf super'
                    `seq`    rnf methods'
    rnf (MethodDescr classDescrM')            =   rnf classDescrM'
    rnf (InstanceDescr binds')                =   rnf binds'
    rnf a                                     =   seq a ()

instance NFData SimpleDescr where
    rnf pd =  rnf (sdName pd)
                    `seq`    rnf (sdType pd)
                    `seq`    rnf (sdLocation pd)
                    `seq`    rnf (sdComment pd)
                    `seq`    rnf (sdExported pd)

instance NFData DescrType where  rnf a = seq a ()

#if !MIN_VERSION_bytestring(0,10,0)
instance NFData ByteString where  rnf b = seq b ()
#endif

#if !MIN_VERSION_deepseq(1,3,0)
instance NFData Version where  rnf v = seq v ()
#endif

instance NFData PackModule where
    rnf pd =  rnf (pack pd)
                    `seq`   rnf (modu pd)

#if !MIN_VERSION_Cabal(2,0,0)
instance NFData ModuleName where
    rnf =  rnf . components
#endif


