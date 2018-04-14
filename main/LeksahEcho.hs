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
import IDE.Utils.VersionUtils (getHaddockVersion, getDefaultGhcVersion, getGhcInfo)
import qualified Data.Text as T (unpack)
import Control.Applicative
import Prelude
import Control.Monad ((>=>))

main :: IO ()
main = do
    args <- getArgs
--    appendFile "/Users/hamish/lecho.log" $ show args ++ "/n"
    if "--version" `elem` args
        then putStrLn =<< T.unpack <$> getHaddockVersion
        else if "--ghc-version" `elem` args
                then putStrLn =<<  getDefaultGhcVersion
                else if "--info" `elem` args
                        then putStrLn =<< T.unpack <$> (getGhcInfo =<< getDefaultGhcVersion)
                        else if "--numeric-version" `elem` args
                                then putStrLn =<<  getDefaultGhcVersion
                                else expandResponseFiles args >>= putStrLn . unwords

expandResponseFiles :: [String] -> IO [String]
expandResponseFiles (('@':responseFile):rest) =
    ((lines >=> words) <$> readFile responseFile) >>= expandResponseFiles . (++ rest)
expandResponseFiles (arg:rest) = (arg:) <$> expandResponseFiles rest
expandResponseFiles [] = return []
