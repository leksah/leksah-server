-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  2007-2011 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- | Windows systems do not often have a real echo executable (so --with-ghc=echo fails)
--
-----------------------------------------------------------------------------

module Main (
main
) where

import System.Environment (getArgs)
import IDE.Utils.VersionUtils (getHaddockVersion, getGhcVersion)

main :: IO ()
main = do
    args <- getArgs
    if elem  "--version" args
        then putStrLn =<<  getHaddockVersion
        else if elem  "--ghc-version" args
                then putStrLn =<<  getGhcVersion
                else if elem  "--numeric-version" args
                        then putStrLn =<<  getGhcVersion
                        else putStrLn $ unwords args

