{-# OPTIONS_GHC -XFlexibleInstances -XDeriveDataTypeable -XExistentialQuantification
    -XMultiParamTypeClasses -XFlexibleContexts -fno-warn-orphans #-}
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
,   RetrieveStrategy(..)

) where

import Data.Typeable (Typeable(..))
import Data.Map (Map)
import Data.Set (Set)
import Default (Default(..))
import MyMissing (nonEmptyLines)
import Distribution.Package
       (PackageName(..), PackageIdentifier(..))
import Distribution.ModuleName (components, ModuleName)
import Data.ByteString.Char8 (ByteString)
import Distribution.Text (simpleParse, display)
import qualified Data.ByteString.Char8 as  BS (unpack, empty)
import qualified Data.Map as Map (lookup,keysSet,splitLookup, insertWith,empty,elems,union,toList)
import qualified Data.Set as Set (toList)
import Text.PrettyPrint as PP
import Text.PrinterParser
import Data.Char (isAlpha)
import Control.DeepSeq (NFData(..))
import qualified Data.ByteString.Char8 as BS (ByteString)
import Data.Version (Version(..))
import PackageConfig (PackageConfig)
import qualified Distribution.InstalledPackageInfo as IPI

-- ---------------------------------------------------------------------
--  | Information about the system, extraced from .hi and source files
--

leksahVersion, configDirName :: String
leksahVersion = "0.12"
configDirName = ".leksah-" ++ leksahVersion

metadataVersion :: Integer
metadataVersion = 7

getThisPackage :: PackageConfig -> PackageIdentifier
#if MIN_VERSION_Cabal(1,8,0)
getThisPackage    =   IPI.sourcePackageId
#else
getThisPackage    =   IPI.package
#endif

data RetrieveStrategy = RetrieveThenBuild | BuildThenRetrieve | NeverRetrieve
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

data ServerCommand =
        SystemCommand {
            scRebuild :: Bool,
            scSources :: Bool,
            scExtract :: Bool}
    |   WorkspaceCommand {
            wcRebuild :: Bool,
            wcPackage :: PackageIdentifier,
            wcPath    :: FilePath,
            wcModList :: [(String,FilePath)]}
    |   ParseHeaderCommand {
            hcFilePath :: FilePath}
    deriving (Eq,Ord,Show,Read)

data ServerAnswer = ServerOK
    | ServerFailed String
    | ServerHeader (Either [ImportDecl] Int)
    deriving (Eq,Ord,Show,Read)


data PackScope  alpha       =   SymbolTable alpha => PackScope (Map PackageIdentifier PackageDescr) alpha
data GenScope           =   forall alpha. SymbolTable alpha  => GenScopeC (PackScope alpha)

class SymbolTable alpha  where
    symLookup       :: String  -> alpha -> [Descr]
    symbols         :: alpha -> Set String
    symSplitLookup  :: String  -> alpha -> (alpha , Maybe [Descr], alpha)
    symInsert       :: String  -> [Descr] -> alpha -> alpha
    symEmpty        :: alpha
    symElems        :: alpha -> [[Descr]]
    symUnion        :: alpha -> alpha -> alpha

instance SymbolTable (Map String [Descr]) where
    symLookup str smap  = case str `Map.lookup` smap of
                                Just dl -> dl
                                Nothing -> []
    symbols             = Map.keysSet
    symSplitLookup      = Map.splitLookup
    symInsert           = Map.insertWith (++)
    symEmpty            = Map.empty
    symElems            = Map.elems
    symUnion            = Map.union

data PackageDescr       =   PackageDescr {
        pdPackage           ::   PackageIdentifier
    ,   pdMbSourcePath      ::   (Maybe FilePath)
    ,   pdModules           ::   [ModuleDescr]
    ,   pdBuildDepends      ::   [PackageIdentifier]
} deriving (Show,Typeable)

instance Default PackageDescr where
    getDefault = PackageDescr getDefault getDefault getDefault getDefault

newtype Present alpha       =   Present alpha

instance Show (Present PackageDescr) where
    show (Present pd)   =   (packageIdentifierToString . pdPackage) pd

instance Eq PackageDescr where
    (== ) a b            =   pdPackage a == pdPackage b

instance Ord PackageDescr where
    (<=) a b              =   pdPackage a <=  pdPackage b

data ModuleDescr        =   ModuleDescr {
        mdModuleId          ::   PackModule
    ,   mdMbSourcePath      ::   (Maybe FilePath)                  -- unqualified
    ,   mdReferences        ::   (Map ModuleName (Set String)) -- imports
    ,   mdIdDescriptions    ::   [Descr]
} deriving (Show,Typeable)

instance Default ModuleDescr where
    getDefault = ModuleDescr getDefault getDefault Map.empty getDefault

instance Show (Present ModuleDescr) where
    show (Present md)   =   (show . mdModuleId) md

instance Eq ModuleDescr where
    (== ) a b            =   mdModuleId a == mdModuleId b

instance Ord ModuleDescr where
    (<=) a b             =   mdModuleId a <=  mdModuleId b

data Descr =  Real RealDescr | Reexported ReexportedDescr
        deriving (Show,Read,Typeable,Eq,Ord)

data RealDescr          =   RealDescr {
        dscName'        ::   String
    ,   dscMbTypeStr'   ::   Maybe ByteString
    ,   dscMbModu'      ::   Maybe PackModule
    ,   dscMbLocation'  ::   Maybe Location
    ,   dscMbComment'   ::   Maybe ByteString
    ,   dscTypeHint'    ::   TypeDescr
    ,   dscExported'    ::   Bool
    }
        deriving (Show,Read,Typeable)

data ReexportedDescr    =   ReexportedDescr {
        dsrMbModu       ::   Maybe PackModule
    ,   dsrDescr        ::   Descr}
        deriving (Show,Read,Typeable)

-- Metadata accessors

isReexported :: Descr -> Bool
isReexported (Reexported _)     =   True
isReexported _                  =   False

dscName :: Descr -> String
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
    |   ClassDescr  [String] [SimpleDescr] -- ^ first super, then methods
    |   MethodDescr Descr -- ^ classDescr
    |   InstanceDescr [String] -- ^ binds
    |   KeywordDescr
    |   ExtensionDescr
    |   ModNameDescr
    |   QualModNameDescr
    |   ErrorDescr
            --the descrName is the type Konstructor?
        deriving (Show,Read,Eq,Ord,Typeable)

data DescrType = Variable | Field | Constructor | Data  | Type | Newtype
    | Class | Method | Instance | Keyword | Extension | ModName | QualModName | Error
  deriving (Show, Eq, Ord, Bounded, Enum, Read)

instance Default DescrType where
    getDefault = Variable

data SimpleDescr = SimpleDescr {
    sdName      :: String,
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

data PackModule         =   PM {    pack :: PackageIdentifier
                                ,   modu :: ModuleName}
                                deriving (Eq, Ord,Read,Show,Typeable)

instance Show (Present PackModule) where
    showsPrec _ (Present pd)  =   showString ((packageIdentifierToString . pack) pd) . showChar ':'
                                    .  showString (display (modu pd))

parsePackModule         ::   String -> PackModule
parsePackModule str     =   let (pack',mod') = span (\c -> c /= ':') str
                            in case packageIdentifierFromString $ pack' of
                                Nothing -> perror $ "Types>>parsePackModule: Can't parse package:" ++ str
                                Just pi'-> case simpleParse $ tail mod' of
                                            Nothing -> perror $
                                                "Types>>parsePackModule: Can't parse module:" ++ str
                                            Just mn -> (PM pi' mn)
    where perror s      =   error $ "cannot parse PackModule from " ++ s

showPackModule :: PackModule -> String
showPackModule              = show. Present

packageIdentifierToString :: PackageIdentifier -> String
packageIdentifierToString   = display

packageIdentifierFromString :: String -> Maybe PackageIdentifier
packageIdentifierFromString = simpleParse

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
                                where nelines = nonEmptyLines (BS.unpack com)
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
    getDefault = parsePackModule "unknow-0:Undefined"

instance Default PackageIdentifier where
    getDefault = case packageIdentifierFromString "unknown-0" of
                    Nothing -> error "CTypes.getDefault: Can't parse Package Identifier"
                    Just it -> it

-- | A portion of the source, spanning one or more lines and zero or more columns.
data SrcSpan = SrcSpan
    { srcSpanFilename    :: String
    , srcSpanStartLine   :: Int
    , srcSpanStartColumn :: Int
    , srcSpanEndLine     :: Int
    , srcSpanEndColumn   :: Int
    }
  deriving (Eq,Ord,Show)

data Location           =   Location {
    locationSLine       ::   Int
,   locationSCol	    ::   Int
,   locationELine       ::   Int
,   locationECol        ::   Int
}   deriving (Show,Eq,Ord,Read,Typeable)

instance Default ByteString
    where getDefault = BS.empty

data Scope = PackageScope Bool | WorkspaceScope Bool | SystemScope
    -- True -> with imports, False -> without imports
  deriving (Show, Eq, Read)

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
    , importModule :: String   -- ^ name of the module imported.
    , importQualified :: Bool          -- ^ imported @qualified@?
    , importSrc :: Bool                -- ^ imported with @{-\# SOURCE \#-}@?
    , importPkg :: Maybe String        -- ^ imported with explicit package name
    , importAs :: Maybe String -- ^ optional alias name in an @as@ clause.
    , importSpecs :: Maybe ImportSpecList
            -- ^ optional list of import specifications.
    }
  deriving (Eq,Ord,Read,Show)

instance Pretty ImportDecl where
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
     = IVar String                  -- ^ variable
     | IAbs String                 -- ^ @T@:
                                        --   the name of a class, datatype or type synonym.
     | IThingAll String             -- ^ @T(..)@:
                                        --   a class imported with all of its methods, or
                                        --   a datatype imported with all of its constructors.
     | IThingWith String [String]  -- ^ @T(C_1,...,C_n)@:
                                        --   a class imported with some of its methods, or
                                        --   a datatype imported with some of its constructors.
  deriving (Eq,Ord,Read,Show)

newtype VName = VName String

instance Pretty ImportSpec where
	pretty (IVar name)                = pretty (VName name)
	pretty (IAbs name)                = pretty name
	pretty (IThingAll name)           = pretty name <> text "(..)"
	pretty (IThingWith name nameList) =
		pretty name <> (parenList (map (pretty.VName) nameList))
		
instance Pretty VName  where
    pretty (VName str) = if isOperator str then PP.parens (PP.text str) else (PP.text str)

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

instance NFData PackageIdentifier where
    rnf pd =  rnf (pkgName pd)
                    `seq`    rnf (pkgVersion pd)

instance NFData DescrType where  rnf a = seq a ()

instance NFData BS.ByteString where  rnf b = seq b ()

#if !MIN_VERSION_ghc(7,3,0)
instance NFData Version where  rnf v = seq v ()
#endif

instance NFData PackModule where
    rnf pd =  rnf (pack pd)
                    `seq`   rnf (modu pd)

instance NFData ModuleName where
    rnf =  rnf . components

instance NFData PackageName where
    rnf (PackageName s) =  rnf s



