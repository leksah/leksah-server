{-# OPTIONS_GHC -XFlexibleInstances -XDeriveDataTypeable -XExistentialQuantification
    -XMultiParamTypeClasses -XFlexibleContexts #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Core.CTypes
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

,   configDirName
,   metadataVersion
,   standardPort

,   ImportDecl(..)
,   ImportSpecList(..)
,   ImportSpec(..)

) where

-- import GHC.ConsoleHandler (Handler(..))
import Data.Typeable (Typeable(..))
import Data.Map (Map(..))
import Data.Set (Set(..))
import Default (Default(..))
import MyMissing (nonEmptyLines)
import Distribution.Package (PackageIdentifier(..))
import Distribution.ModuleName (ModuleName(..))
import Data.ByteString.Char8 (ByteString(..))
import Distribution.Text (simpleParse, display)
import qualified Data.ByteString.Char8 as  BS (unpack, empty)
import qualified Data.Map as Map (lookup,keysSet,splitLookup, insertWith,empty,elems,union)

-- ---------------------------------------------------------------------
--  | Information about the system, extraced from .hi and source files
--

configDirName = ".leksah-0.7"

metadataVersion :: Integer
metadataVersion = 7

standardPort = 80

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
    | ServerHeader (Maybe [ImportDecl])
    deriving (Eq,Ord,Show,Read)

data ImportDecls = ImportDecls
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
    symLookup str map  = case str `Map.lookup` map of
                                Just dl -> dl
                                Nothing -> []
    symbols            = Map.keysSet
    symSplitLookup     = Map.splitLookup
    symInsert          = Map.insertWith (++)
    symEmpty           = Map.empty
    symElems           = Map.elems
    symUnion           = Map.union

data PackageDescr       =   PackageDescr {
        pdPackage           ::   PackageIdentifier
    ,   pdMbSourcePath      ::   (Maybe FilePath)
    ,   pdModules           ::   [ModuleDescr]
    ,   pdBuildDepends      ::   [PackageIdentifier]
} deriving (Show,Typeable)

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

dscMbModu :: Descr -> Maybe PackModule
dscMbModu (Reexported d)        = dscMbModu (dsrDescr d)
dscMbModu (Real d)              = dscMbModu' d

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
dscExported (Reexported d)      = True
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
        where p         =   case dscMbModu descr of
                                Just ds -> showString "-- " . shows (Present ds)
                                Nothing -> id
              c com     =   showString $ unlines
                                $ map (\(i,l) -> if i == 0 then "-- | " ++ l else "--  " ++ l)
                                    $ zip [0 .. length lines - 1] lines
                                where lines = nonEmptyLines (BS.unpack com)
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

