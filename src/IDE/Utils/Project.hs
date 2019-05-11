{-# LANGUAGE DeriveGeneric #-}
module IDE.Utils.Project where

import Data.Aeson (ToJSON(..), FromJSON(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import System.FilePath (dropFileName, takeExtension)

newtype CabalProject = CabalProject
  { pjCabalFile :: FilePath
  } deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON CabalProject
instance FromJSON CabalProject

newtype StackProject = StackProject
  { pjStackFile :: FilePath
  } deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON StackProject
instance FromJSON StackProject

data CustomProject = CustomProject
  { pjCustomDir        :: FilePath
  , pjCustomNixShell   :: [Text]
  , pjCustomGhcBuild   :: Maybe (FilePath, [Text])
  , pjCustomGhcjsBuild :: Maybe (FilePath, [Text])
  , pjCustomDoc        :: Maybe (FilePath, [Text])
  , pjCustomRepl       :: Maybe (FilePath, [Text])
  } deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON CustomProject
instance FromJSON CustomProject

data ProjectKey =
    CabalTool CabalProject
  | StackTool StackProject
  | CustomTool CustomProject
  deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON ProjectKey
instance FromJSON ProjectKey

pjDir :: ProjectKey -> FilePath
pjDir (CabalTool p)  = dropFileName (pjCabalFile p)
pjDir (StackTool p)  = dropFileName (pjStackFile p)
pjDir (CustomTool p) = pjCustomDir p

pjFile :: ProjectKey -> Maybe FilePath
pjFile (CabalTool p)  = Just $ pjCabalFile p
pjFile (StackTool p)  = Just $ pjStackFile p
pjFile _ = Nothing

pjFileOrDir :: ProjectKey -> FilePath
pjFileOrDir (CabalTool p)  = pjCabalFile p
pjFileOrDir (StackTool p)  = pjStackFile p
pjFileOrDir (CustomTool p) = pjCustomDir p

pjIsCabal :: ProjectKey -> Bool
pjIsCabal (CabalTool _) = True
pjIsCabal _ = False

pjIsStack :: ProjectKey -> Bool
pjIsStack (StackTool _) = True
pjIsStack _ = False

filePathToProjectKey :: FilePath -> Maybe ProjectKey
filePathToProjectKey filePath =
  case takeExtension filePath of
    ".project" -> Just (CabalTool (CabalProject filePath))
    ".yaml" -> Just (StackTool (StackProject filePath))
    _ -> Nothing

