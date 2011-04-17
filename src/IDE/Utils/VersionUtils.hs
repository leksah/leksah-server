{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
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
,   getGhcVersion
) where

import IDE.Utils.Tool (toolline, runTool')
import Data.Char (ord)
import qualified Data.List as List (init)
import System.Log.Logger (debugM)

getGhcVersion :: IO FilePath
getGhcVersion = catch (do
    (!output,_) <- runTool' "ghc" ["--numeric-version"] Nothing
    let vers = toolline $ head output
        vers2 = if ord (last vers) == 13
                    then List.init vers
                    else vers
    debugM "leksah-server" $ "Got GHC Version " ++ vers2
    return vers2
    ) $ \ _ -> error ("FileUtils>>getGhcVersion failed")

getHaddockVersion :: IO String
getHaddockVersion = catch (do
    (!output,_) <- runTool' "haddock" ["--version"] Nothing
    let vers = toolline $ head output
        vers2 = if ord (last vers) == 13
                    then List.init vers
                    else vers
    return vers2
    ) $ \ _ -> error ("FileUtils>>getHaddockVersion failed")



