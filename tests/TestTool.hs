{-# LANGUAGE CPP #-}
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
import System.Exit (exitWith, exitSuccess, exitFailure, ExitCode(..))
import IDE.Utils.Tool
       (toolProcess, executeGhciCommand, ToolOutput(..), runTool',
        newGhci')
#ifdef MIN_VERSION_process_leksah
import IDE.System.Process (interruptProcessGroup, getProcessExitCode)
#else
import System.Process (interruptProcessGroupOf, getProcessExitCode)
#endif
import Test.HUnit
       ((@=?), (@?=), putTextToHandle, Counts(..), runTestTT, assertBool,
        runTestText, (~:), Testable(..), Test(..))
import System.IO (hPutStr, stdout, hPutStrLn, stderr)
import qualified Data.Enumerator.List as EL (consume)
import Control.Concurrent
       (threadDelay, forkIO, takeMVar, putMVar, newEmptyMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)
import System.Log.Logger
       (setLevel, rootLoggerName, updateGlobalLogger)
import System.Log (Priority(..))

runSelf' args = runTool' "dist/build/test-tool/test-tool" args Nothing

-- stderr and stdout may not be in sync
check output expected = do
    checkFiltered notOut
    checkFiltered notErr
  where
    checkFiltered f = filter f output @?= filter f expected
    notErr (ToolError _) = False
    notErr _ = True
    notOut (ToolOutput _) = False
    notOut _ = True

runTests testMVar = loop
  where
    loop = do
        mbTest <- takeMVar testMVar
        case mbTest of
            Just test -> do
                test
                loop
            Nothing   -> return ()

sendTest testMVar test = do
    liftIO $ putMVar testMVar $ Just test

doneTesting testMVar = do
    liftIO $ putMVar testMVar $ Nothing

tests = test [
    "Exit Success" ~: do
        (output, _) <- runSelf' ["ExitSuccess"]
        output `check` [ToolExit ExitSuccess],
    "Exit Failure" ~: do
        (output, _) <- runSelf' ["Exit42"]
        output `check` [ToolExit (ExitFailure 42)],
    "Single Blank Out Line" ~: do
        (output, _) <- runSelf' ["BlankLine", "StdOut"]
        output `check` [ToolOutput "", ToolExit ExitSuccess],
    "Single Blank Err Line" ~: do
        (output, _) <- runSelf' ["BlankLine", "StdErr"]
        output `check` [ToolError "", ToolExit ExitSuccess],
    "Hello Out" ~: do
        (output, _) <- runSelf' ["Hello", "StdOut"]
        output `check` [ToolOutput "Hello World", ToolExit ExitSuccess],
    "Hello Err" ~: do
        (output, _) <- runSelf' ["Hello", "StdErr"]
        output `check` [ToolError "Hello World", ToolExit ExitSuccess],
    "Both" ~: do
        (output, _) <- runSelf' ["ErrAndOut"]
        output `check` [ToolError "Error", ToolOutput "Output", ToolExit ExitSuccess],
    "Unterminated Out" ~: do
        (output, _) <- runSelf' ["Unterminated", "StdOut"]
        output `check` [ToolOutput "Unterminated", ToolExit ExitSuccess],
    "Unterminated Err" ~: do
        (output, _) <- runSelf' ["Unterminated", "StdErr"]
        output `check` [ToolError "Unterminated", ToolExit ExitSuccess],
    "GHCi" ~: do
        t <- newEmptyMVar
        tool <- newGhci' [] $ do
            output <- EL.consume
            sendTest t $ last output @?= (ToolPrompt "")
        executeGhciCommand tool "1+1" $ do
            output <- EL.consume
            sendTest t $ output `check` [
                ToolInput "1+1",
                ToolOutput "2",
                ToolPrompt ""]
        executeGhciCommand tool "jfkdfjdkl" $ do
            output <- EL.consume
            sendTest t $ output `check` [
                ToolInput "jfkdfjdkl",
                ToolError "",
#if __GLASGOW_HASKELL__ > 702
                ToolError "<interactive>:12:1: Not in scope: `jfkdfjdkl'",
#else
                ToolError "<interactive>:1:1: Not in scope: `jfkdfjdkl'",
#endif
                ToolPrompt ""]
        executeGhciCommand tool "\n1+1" $ do
            output <- EL.consume
            sendTest t $ output `check` [
                ToolInput "",
                ToolInput "1+1",
                ToolOutput "2",
                ToolPrompt ""]
        executeGhciCommand tool ":m + Prelude" $ do
            output <- EL.consume
            sendTest t $ output `check` [
                ToolInput ":m + Prelude",
                ToolPrompt ""]
        executeGhciCommand tool "\njfkdfjdkl" $ do
            output <- EL.consume
            sendTest t $ output `check` [
                ToolInput "",
                ToolInput "jfkdfjdkl",
                ToolError "",
#if __GLASGOW_HASKELL__ > 702
                ToolError "<interactive>:27:1: Not in scope: `jfkdfjdkl'",
#else
                ToolError "<interactive>:1:1: Not in scope: `jfkdfjdkl'",
#endif
                ToolPrompt ""]
        executeGhciCommand tool "do\n putStrLn \"1\"\n putStrLn \"2\"\n putStrLn \"3\"\n putStrLn \"4\"\n putStrLn \"5\"\n" $ do
            output <- EL.consume
            sendTest t $ output `check` [
                ToolInput "do",
                ToolInput " putStrLn \"1\"",
                ToolInput " putStrLn \"2\"",
                ToolInput " putStrLn \"3\"",
                ToolInput " putStrLn \"4\"",
                ToolInput " putStrLn \"5\"",
                ToolOutput "1",
                ToolOutput "2",
                ToolOutput "3",
                ToolOutput "4",
                ToolOutput "5",
                ToolPrompt ""]
        executeGhciCommand tool "do\n putStrLn \"| 1\"\n putStrLn \"| 2\"\n putStrLn \"| 3\"\n putStrLn \"| 4\"\n putStrLn \"| 5\"\n" $ do
            output <- EL.consume
            sendTest t $ output `check` [
                ToolInput "do",
                ToolInput " putStrLn \"| 1\"",
                ToolInput " putStrLn \"| 2\"",
                ToolInput " putStrLn \"| 3\"",
                ToolInput " putStrLn \"| 4\"",
                ToolInput " putStrLn \"| 5\"",
                ToolOutput "| 1",
                ToolOutput "| 2",
                ToolOutput "| 3",
                ToolOutput "| 4",
                ToolOutput "| 5",
                ToolPrompt ""]
        executeGhciCommand tool "putStr \"ABC\"" $ do
            output <- EL.consume
            sendTest t $ output `check` [
                ToolInput "putStr \"ABC\"",
                ToolPrompt "ABC"]
        executeGhciCommand tool ":m +Data.List" $ do
            output <- EL.consume
            sendTest t $ output `check` [
                ToolInput ":m +Data.List",
                ToolPrompt ""]
        executeGhciCommand tool ":quit" $ do
            output <- EL.consume
            sendTest t $ output `check` [
                ToolInput ":quit",
                ToolOutput "Leaving GHCi.",
                ToolExit ExitSuccess]
        runTests t]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            updateGlobalLogger rootLoggerName (\ l -> setLevel DEBUG l)
            (Counts{failures=failures}, _) <- runTestText (putTextToHandle stderr False) tests
            if failures == 0
                then exitSuccess
                else exitFailure
        ["ExitSuccess"]     -> exitSuccess
        ["Exit42"]          -> exitWith (ExitFailure 42)
        ["BlankLine", o]    -> hPutStrLn (h o) ""
        ["Hello", o]        -> hPutStrLn (h o) "Hello World"
        ["ErrAndOut"]       -> hPutStrLn stderr "Error" >> hPutStrLn stdout "Output"
        ["Unterminated", o] -> hPutStr (h o) "Unterminated"
        _  -> exitFailure
 where
    h "StdErr" = stderr
    h _ = stdout
