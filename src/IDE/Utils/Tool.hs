{-# OPTIONS_GHC -XRecordWildCards -XCPP -XBangPatterns -fno-warn-orphans #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Utils.Tool
-- Copyright   :  (c) Hamish Mackenzie, Juergen Nicklisch-Franken
-- License     :  GPL
--
-- Maintainer  :  <maintainer@leksah.org>
-- Stability   :  provisional
-- Portability :
--
-- | Support for running external tools.  Written mainly for GHCi but with
-- | support for others in mind.
--
-----------------------------------------------------------------------------

module IDE.Utils.Tool (
    ToolOutput(..),
    toolline,
    ToolCommand(..),
    ToolState(..),
    toolProcess,
    newToolState,
    runTool,
    runTool',
    runInteractiveTool,
    newGhci,
    executeCommand,
    executeGhciCommand,
    quoteArg,
    escapeQuotes,

--    waitForChildren,
--    forkChild

) where

import Control.Concurrent
       (readMVar, takeMVar, putMVar, newEmptyMVar, forkIO, newChan, MVar,
        Chan, writeChan, getChanContents, dupChan)
import Control.Monad (unless, when)
import Data.List (stripPrefix)
import Data.Maybe (isJust, catMaybes)
#ifdef MIN_VERSION_process_leksah
import IDE.System.Process
       (proc, waitForProcess, ProcessHandle, createProcess, CreateProcess(..))
import IDE.System.Process.Internals (StdStream(..))
#else
import System.Process
       (proc, waitForProcess, ProcessHandle, createProcess, CreateProcess(..))
import System.Process.Internals (StdStream(..))
#endif
import Control.DeepSeq
import System.Log.Logger (debugM, criticalM)
import System.Exit (ExitCode(..))
import System.IO (hGetContents, hFlush, hPutStrLn, Handle)
import Control.Applicative ((<$>))
import Data.Char (isNumber)

data ToolOutput = ToolInput String
                | ToolError String
                | ToolOutput String
                | ToolPrompt
                | ToolExit ExitCode deriving(Eq, Show)

instance NFData ExitCode where
    rnf ExitSuccess = rnf ()
    rnf (ExitFailure failureCode) = rnf failureCode

instance  NFData ToolOutput where
    rnf (ToolInput s) = rnf s
    rnf (ToolError s) = rnf s
    rnf (ToolOutput s) = rnf s
    rnf (ToolPrompt) = rnf ()
    rnf (ToolExit code) = rnf code

data ToolCommand = ToolCommand String ([ToolOutput] -> IO ())
data ToolState = ToolState {
    toolProcessMVar :: MVar ProcessHandle,
    outputClosed :: MVar Bool,
    toolCommands :: Chan ToolCommand,
    toolCommandsRead :: Chan ToolCommand,
    currentToolCommand :: MVar ToolCommand}

toolProcess :: ToolState -> IO ProcessHandle
toolProcess = readMVar . toolProcessMVar

data RawToolOutput = RawToolOutput ToolOutput
                   | ToolOutClosed
                   | ToolErrClosed
                   | ToolClosed deriving(Eq, Show)

toolline :: ToolOutput -> String
toolline (ToolInput l)  = l
toolline (ToolOutput l) = l
toolline (ToolError l)  = l
toolline (ToolPrompt)  = ""
toolline (ToolExit _code) = ""

quoteArg :: String -> String
quoteArg s | ' ' `elem` s = "\"" ++ (escapeQuotes s) ++ "\""
quoteArg s                = s

escapeQuotes :: String -> String
escapeQuotes = foldr (\c s -> if c == '"' then '\\':c:s else c:s) ""

runTool' :: FilePath -> [String] -> Maybe FilePath -> IO ([ToolOutput], ProcessHandle)
runTool' fp args mbDir = do
    debugM "leksah-server" $ "Start: " ++ show (fp, args)
    (out,pid) <- runTool fp args mbDir
    deepseq out $ waitForProcess pid
    debugM "leksah-server" $ "End: " ++ show (fp, args)
    return (out,pid)

runTool :: FilePath -> [String] -> Maybe FilePath -> IO ([ToolOutput], ProcessHandle)
runTool executable arguments mbDir = do
    (Just inp,Just out,Just err,pid) <- createProcess (proc executable arguments)
        { std_in  = CreatePipe,
          std_out = CreatePipe,
          std_err = CreatePipe,
          cwd = mbDir,
          new_group = True }
    output <- getOutputNoPrompt inp out err pid
    return (output, pid)

newToolState :: IO ToolState
newToolState = do
    toolProcessMVar <- newEmptyMVar
    outputClosed <- newEmptyMVar
    toolCommands <- newChan
    toolCommandsRead <- dupChan toolCommands
    currentToolCommand <- newEmptyMVar
    return ToolState{..}

runInteractiveTool ::
    ToolState ->
    CommandLineReader ->
    FilePath ->
    [String] ->
    IO ()
runInteractiveTool tool clr executable arguments = do
    (Just inp,Just out,Just err,pid) <- createProcess (proc executable arguments)
        { std_in  = CreatePipe,
          std_out = CreatePipe,
          std_err = CreatePipe,
          new_group = True }
    putMVar (toolProcessMVar tool) pid
    output <- getOutput clr inp out err pid
    -- This is handy to show the processed output
    -- forkIO $ forM_ output (putStrLn.show)
    forkIO $ do
        commands <- getChanContents (toolCommandsRead tool)
        processCommand 0 commands inp output
    return ()
    where
        processCommand _ [] _ _ = do
            debugM "leksah-server" $ "No More Commands"
            return ()
        processCommand n ((command@(ToolCommand commandString handler)):remainingCommands)
                inp allOutput = do
            putMVar (currentToolCommand tool) command
            hPutStrLn inp commandString
            hFlush inp
            outputChan <- newChan
            outputChan' <- dupChan outputChan

            done <- newEmptyMVar
            forkIO $ do
                output <- fromRawOutput <$> getChanContents outputChan'
                debugM "leksah-server" $ "Start Processing Tool Output for " ++ commandString
                handler $ (map ToolInput (lines commandString)) ++ output
                debugM "leksah-server" $ "Done Processing Tool Output for " ++ commandString
                putMVar done True
                return ()

            remainingOutputWithPrompt <- writeCommandOutput outputChan inp allOutput False False (outputSyncCommand clr) n
            takeMVar done

            takeMVar (currentToolCommand tool)

            case remainingOutputWithPrompt of
                (RawToolOutput ToolPrompt:remainingOutput) -> do
                    debugM "leksah-server" $ "Ready For Next Command"
                    processCommand (n+1) remainingCommands inp remainingOutput
                [] -> do
                    debugM "leksah-server" $ "Tool Output Closed"
                    putMVar (outputClosed tool) True
                _ -> do
                    criticalM "leksah-server" $ "This should never happen in Tool.hs"

        writeCommandOutput _ _ [] _ _ _ _ = do
            criticalM "leksah-server" $ "ToolExit not found"
            return []

        writeCommandOutput out inp (RawToolOutput ToolPrompt:remainingOutput) False False (Just outSyncCmd) n = do
            debugM "leksah-server" $ "Pre Sync Prompt"
            hPutStrLn inp $ outSyncCmd n
            hFlush inp
            writeCommandOutput out inp remainingOutput True False (Just outSyncCmd) n

        writeCommandOutput out inp (RawToolOutput ToolPrompt:remainingOutput) True False (Just outSyncCmd) n = do
            debugM "leksah-server" $ "Unsynced Prompt"
            writeCommandOutput out inp remainingOutput True False (Just outSyncCmd) n

        writeCommandOutput out inp (o@(RawToolOutput (ToolOutput line)):remainingOutput) True False (Just outSyncCmd) n = do
            let synced = (isExpectedOutput clr n line)
            unless synced $ writeChan out o
            when synced $ debugM "leksah-server" $ "Output Sync Found"
            writeCommandOutput out inp remainingOutput True synced (Just outSyncCmd) n

        writeCommandOutput out _ remainingOutput@(RawToolOutput ToolPrompt:_) _ _ _ _ = do
            debugM "leksah-server" $ "Synced Prompt"
            writeChan out $ RawToolOutput ToolPrompt
            return remainingOutput

        writeCommandOutput out _ (o@(RawToolOutput (ToolExit _)):_) _ _ _ _ = do
            debugM "leksah-server" $ "Tool Exit"
            writeChan out o
            return []

        writeCommandOutput out inp (o:remainingOutput) synching synched syncCmd n = do
            writeChan out o
            writeCommandOutput out inp remainingOutput synching synched syncCmd n

{-
newInteractiveTool :: (Handle -> Handle -> Handle -> ProcessHandle -> IO [RawToolOutput]) -> FilePath -> [String] -> IO ToolState
newInteractiveTool getOutput' executable arguments = do
    tool <- newToolState
    runInteractiveTool tool getOutput' executable arguments
    return tool
-}

ghciPrompt :: String
ghciPrompt = "3KM2KWR7LZZbHdXfHUOA5YBBsJVYoCQnKX"

data CommandLineReader = CommandLineReader {
    stripInitialPrompt :: String -> Maybe String,
    stripFollowingPrompt :: String -> Maybe String,
    errorSyncCommand :: Maybe (Int -> String),
    stripExpectedError :: String -> Maybe (Int, String),
    outputSyncCommand :: Maybe (Int -> String),
    isExpectedOutput :: Int -> String -> Bool
    }

ghciStripInitialPrompt :: String -> Maybe String
ghciStripInitialPrompt output =
    case catMaybes [stripPrefix "Prelude" output, stripPrefix "*" output] of
        remaining:_ ->
            case dropWhile (/= '>') remaining of
                '>':' ':next -> Just next
                _ -> Nothing
        _ -> Nothing

ghciStripFollowingPrompt :: String -> Maybe String
ghciStripFollowingPrompt = stripPrefix ghciPrompt

{-
stripMarker $ marker 0 ++ "dfskfjdkl"
-}

marker :: Int -> String
marker n =
        take (29 - length num) "kMAKWRALZZbHdXfHUOAAYBBsJVYoC" ++ num
    where num = show n

stripMarker :: String -> Maybe (Int, String)
stripMarker s =
        case strip "kMAKWRALZZbHdXfHUOAAYBBsJVYoC" s of
            Just (nums, rest) -> Just (read nums, rest)
            Nothing -> Nothing
    where
        strip :: String -> String -> Maybe (String, String)
        strip letters@(a:as) input@(b:bs)
            | a == b = strip as bs
            | otherwise = numbers letters input
        strip _ _ = Nothing
        numbers :: String -> String -> Maybe (String, String)
        numbers (_:as) (n:ns)
            | isNumber n = case numbers as ns of
                                Just (nums, rest) -> Just (n:nums, rest)
                                _      -> Nothing
            | otherwise = Nothing
        numbers [] input = Just ([], input)
        numbers _ _ = Nothing



ghciStripExpectedError :: String -> Maybe (Int, String)
ghciStripExpectedError output =
    case stripPrefix "\n<interactive>:1:0" output of
        Just rest ->
            case stripPrefix ": Not in scope: `"
                (maybe rest id (stripPrefix "-28" rest)) of
                Just rest2 ->
                    case stripMarker rest2 of
                        Just (n, rest3) ->
                            case stripPrefix "'\n" rest3 of
                                Just rest4 -> Just (n, rest4)
                                Nothing -> Nothing
                        Nothing -> Nothing
                Nothing -> Nothing
        Nothing -> Nothing

ghciIsExpectedOutput :: Int -> String -> Bool
ghciIsExpectedOutput n =
    (==) (marker n)

ghciCommandLineReader :: CommandLineReader
ghciCommandLineReader    = CommandLineReader {
    stripInitialPrompt   = ghciStripInitialPrompt,
    stripFollowingPrompt = ghciStripFollowingPrompt,
    errorSyncCommand     = Just $ \n -> marker n,
    stripExpectedError   = ghciStripExpectedError,
    outputSyncCommand    = Just $ \n -> ":set prompt " ++ marker n ++ "\n:set prompt " ++ ghciPrompt,
    isExpectedOutput     = ghciIsExpectedOutput
    }

noInputCommandLineReader :: CommandLineReader
noInputCommandLineReader = CommandLineReader {
    stripInitialPrompt = const Nothing,
    stripFollowingPrompt = const Nothing,
    errorSyncCommand = Nothing,
    stripExpectedError = \_ -> Nothing,
    outputSyncCommand = Nothing,
    isExpectedOutput = \_ _ -> False
    }
--waitTillEmpty :: Handle -> IO ()
--waitTillEmpty handle = do
--    ready <- hReady handle
--    when ready $ do
--        yield
--        threadDelay 100
--        yield
--        waitTillEmpty handle

getOutput :: CommandLineReader -> Handle -> Handle -> Handle -> ProcessHandle -> IO [RawToolOutput]
getOutput clr inp out err pid = do
    chan <- newChan
    testClosed <- dupChan chan
    -- hSetBuffering out NoBuffering
    -- hSetBuffering err NoBuffering
    foundExpectedError <- newEmptyMVar
    -- Use this and the too putStr threads bellow if you want to see the raw output
    -- hSetBuffering stdout NoBuffering
    forkIO $ do
        errors <- hGetContents err
        -- forkIO $ putStr errors
        readError chan (filter (/= '\r') errors) foundExpectedError
        writeChan chan ToolErrClosed
    forkIO $ do
        output <- hGetContents out
        -- forkIO $ putStr output
        readOutput chan (filter (/= '\r') output) 0 foundExpectedError False
        writeChan chan ToolOutClosed
    forkIO $ do
        output <- getChanContents testClosed
        when ((ToolOutClosed `elem` output) && (ToolErrClosed `elem` output)) $ do
            exitCode <- waitForProcess pid
            writeChan chan (RawToolOutput (ToolExit exitCode))
            writeChan chan ToolClosed
            debugM "leksah-server" $ "Tool Exited " ++ show exitCode
    fmap (takeWhile ((/=) ToolClosed)) $ getChanContents chan
    where
        readError chan errors foundExpectedError = do
            case stripExpectedError clr errors of
                Just (counter, unexpectedErrors) -> do
                    putMVar foundExpectedError counter
                    readError chan unexpectedErrors foundExpectedError
                Nothing -> do
                    let (line, remaining) = break (== '\n') errors
                    case remaining of
                        [] -> return ()
                        _:remainingLines -> do
                            writeChan chan $ RawToolOutput $ ToolError line
                            readError chan remainingLines foundExpectedError

        readOutput chan output counter foundExpectedError errSynced = do
            let stripPrompt = (if counter==0 then (stripInitialPrompt clr) else (stripFollowingPrompt clr))
            let line = getOutputLine stripPrompt output
            let remaining = drop (length line) output
            case remaining of
                [] -> do
                        when (line /= "") $ writeChan chan $ RawToolOutput $ ToolOutput line
                '\n':remainingLines -> do
                        writeChan chan $ RawToolOutput $ ToolOutput line
                        readOutput chan remainingLines counter foundExpectedError errSynced
                _ -> do
                    when (line /= "") $ writeChan chan $ RawToolOutput $ ToolOutput line
                    case stripPrompt remaining of
                        Just afterPrompt -> do
                            case (counter, errSynced, errorSyncCommand clr) of
                                (0, _, _) -> do
                                    readOutput chan afterPrompt (counter+1) foundExpectedError errSynced

                                (_, False, Just syncCmd) -> do
                                    hPutStrLn inp $ syncCmd counter
                                    hFlush inp
                                    waitForError counter foundExpectedError
                                    readOutput chan afterPrompt (counter+1) foundExpectedError True

                                _ -> do
                                    writeChan chan $ RawToolOutput ToolPrompt
                                    readOutput chan afterPrompt (counter+1) foundExpectedError False

                        _ -> return () -- Should never happen
        getOutputLine _ [] = []
        getOutputLine _ ('\n':_) = []
        getOutputLine stripPrompt output@(x:xs)
            | isJust (stripPrompt output) = []
            | otherwise                   = x : (getOutputLine stripPrompt xs)
        waitForError counter foundExpectedError = do
            foundCount <- takeMVar foundExpectedError
            when (foundCount < counter) $ waitForError counter foundExpectedError


fromRawOutput :: [RawToolOutput] -> [ToolOutput]
fromRawOutput [] = []
fromRawOutput (RawToolOutput (ToolPrompt):_) = [ToolPrompt]
fromRawOutput (RawToolOutput (ToolExit code):_) = [ToolExit code]
fromRawOutput (RawToolOutput output:xs) = output : (fromRawOutput xs)
fromRawOutput (_:xs) = fromRawOutput xs

getOutputNoPrompt :: Handle -> Handle -> Handle -> ProcessHandle -> IO [ToolOutput]
getOutputNoPrompt inp out err pid = fmap fromRawOutput $ getOutput noInputCommandLineReader inp out err pid

newGhci :: [String] -> [String] -> ([ToolOutput] -> IO ()) -> IO ToolState
newGhci buildFlags interactiveFlags startupOutputHandler = do
        tool <- newToolState
        writeChan (toolCommands tool) $
            ToolCommand (":set prompt " ++ ghciPrompt) startupOutputHandler
        debugM "leksah-server" $ "Working out GHCi options"
        forkIO $ do
            (output, _) <- runTool "runhaskell" (["Setup","build","--with-ghc=leksahecho"] ++ buildFlags) Nothing
            case catMaybes $ map (findMake . toolline) output of
                options:_ -> do
                        let newOptions = filterUnwanted options
                        debugM "leksah-server" $ newOptions
                        debugM "leksah-server" $ "Starting GHCi"
                        debugM "leksah-server" $ unwords (words newOptions ++ ["-fforce-recomp"] ++ interactiveFlags)
                        runInteractiveTool tool ghciCommandLineReader "ghci"
                            (words newOptions ++ ["-fforce-recomp"] ++ interactiveFlags)
                _ -> do
                    startupOutputHandler output
                    putMVar (outputClosed tool) True
        return tool
    where
        findMake [] = Nothing
        findMake line@(_:xs) =
                case stripPrefix "--make " line of
                    Nothing -> findMake xs
                    s -> s
        filterUnwanted [] = []
        filterUnwanted line@(x:xs) =
                case stripPrefix "-O " line of
                    Nothing -> x: filterUnwanted xs
                    Just s  -> filterUnwanted s


executeCommand :: ToolState -> String -> ([ToolOutput] -> IO ()) -> IO ()
executeCommand tool command handler = do
    writeChan (toolCommands tool) $ ToolCommand command handler

executeGhciCommand :: ToolState -> String -> ([ToolOutput] -> IO ()) -> IO ()
executeGhciCommand tool command handler = do
    if '\n' `elem` command
        then executeCommand tool safeCommand $ \output -> do
            handler $ fixInput $ fixOutput output
        else executeCommand tool command handler
    where
        filteredLines = (filter safeLine (lines command))
        promptCount = (length filteredLines)+1
        safeCommand = (unlines ([":{"] ++ filteredLines)) ++ ":}"
        safeLine ":{" = False
        safeLine ":}" = False
        safeLine _ = True
        fixOutput ((ToolOutput line):xs) = (ToolOutput (removePrompts line line promptCount)):xs
        fixOutput (x:xs) = x:(fixOutput xs)
        fixOutput [] = []
        fixInput = filter (\x -> (x /= ToolInput ":{") && (x /= ToolInput ":}"))
        removePrompts _fullLine line 0 = line
        removePrompts fullLine line n = case dropWhile ((/=) '|') line of
            '|':' ':xs -> removePrompts fullLine xs (n-1)
            _ -> fullLine

--children :: MVar [MVar ()]
--children = unsafePerformIO (newMVar [])
--
--waitForChildren :: IO ()
--waitForChildren = do
--  cs <- takeMVar children
--  case cs of
--    []   -> return ()
--    m:ms -> do
--       putMVar children ms
--       takeMVar m
--       waitForChildren
--
--forkChild :: IO () -> IO ThreadId
--forkChild io = do
--    mvar <- newEmptyMVar
--    childs <- takeMVar children
--    putMVar children (mvar:childs)
--    forkIO (io `finally` putMVar mvar ())

