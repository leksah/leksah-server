{-# LANGUAGE CPP, ScopedTypeVariables, BangPatterns, OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Utils.VersionUtils
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL Nothing
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module IDE.Utils.VersionUtils (
    getHaddockVersion
,   getDefaultGhcVersion
,   getGhcInfo
,   supportedGhcVersions
,   ghcExeName
) where

import Prelude ()
import Prelude.Compat
import IDE.Utils.Tool (ToolOutput(..), runTool')
import Data.Char (ord)
import Data.List (nub)
import System.Log.Logger (debugM)
import Control.Exception as E (SomeException, catch)
import qualified Data.Text as T (unlines, unpack, init, last)
import Data.Text (Text)
import Control.DeepSeq (deepseq)
import GHC.Stack (HasCallStack)

supportedGhcVersions :: [FilePath]
supportedGhcVersions = nub ["8.0.2", "8.2.2", VERSION_ghc]

getDefaultGhcVersion :: HasCallStack => IO FilePath
getDefaultGhcVersion = E.catch (do
    (!output,_) <- runTool' "ghc" ["--numeric-version"] Nothing Nothing
    let vers = head [l | ToolOutput l <- output]
        vers2 = if ord (T.last vers) == 13
                    then T.init vers
                    else vers
    debugM "leksah-server" $ "Got GHC Version " ++ T.unpack vers2
    output `deepseq` return $ T.unpack vers2
    ) $ \ (e :: SomeException) -> do
      debugM "leksah-server" $ "No `ghc` in path defaulting to " ++ VERSION_ghc
      return VERSION_ghc

ghcExeName :: Maybe FilePath -> FilePath
#ifdef mingw32_HOST_OS
ghcExeName Nothing = "ghc.exe"
ghcExeName (Just ver) = "ghc-" <> ver <> ".exe"
#else
ghcExeName = maybe "ghc" ("ghc-" <>)
#endif

getGhcInfo :: Maybe FilePath -> IO Text
getGhcInfo ver = E.catch (do
    (!output,_) <- runTool' (ghcExeName ver) ["--info"] Nothing Nothing
    output `deepseq` return $ T.unlines [l | ToolOutput l <- output]
    ) $ \ (e :: SomeException) -> error $ "FileUtils>>getGhcInfo failed with " ++ show e

getHaddockVersion :: IO Text
getHaddockVersion = E.catch (do
    (!output,_) <- runTool' "haddock" ["--version"] Nothing Nothing
    let vers = head [l | ToolOutput l <- output]
        vers2 = if ord (T.last vers) == 13
                    then T.init vers
                    else vers
    output `deepseq` return vers2
    ) $ \ (e :: SomeException) -> error $ "FileUtils>>getHaddockVersion failed with " ++ show e



