{-# LANGUAGE OverloadedStrings #-}
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
    newGhci',
    executeCommand,
    executeGhciCommand,
    quoteArg,
    escapeQuotes,
    runCommand,
    waitForProcess,
    interruptProcessGroupOf,
    ProcessHandle,
    getProcessExitCode,
    runInteractiveProcess,
    runProcess,
    readProcessWithExitCode,
    terminateProcess

--    waitForChildren,
--    forkChild

) where

import Control.Concurrent
       (tryTakeMVar, readMVar, takeMVar, putMVar,
        newEmptyMVar, forkIO, newChan, MVar, Chan, writeChan,
        getChanContents, dupChan)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Maybe (maybeToList)
#ifdef MIN_VERSION_process_leksah
import IDE.System.Process
       (proc, waitForProcess, ProcessHandle, createProcess, CreateProcess(..),
       interruptProcessGroup, runCommand, getProcessExitCode,
       runProcess, runInteractiveProcess, readProcessWithExitCode,
       terminateProcess)
import IDE.System.Process.Internals (StdStream(..))
#else
import System.Process
       (proc, waitForProcess, ProcessHandle, createProcess, CreateProcess(..),
       interruptProcessGroupOf, runCommand, getProcessExitCode,
       runProcess, runInteractiveProcess, readProcessWithExitCode,
       terminateProcess)
import System.Process.Internals (StdStream(..))
#endif
import qualified Data.Text as T (null, lines, any, unpack, pack)
import Control.DeepSeq
import System.Log.Logger (debugM)
import System.Exit (ExitCode(..))
import System.IO
       (hClose, hFlush, Handle, hSetBuffering, BufferMode(..))
import Control.Applicative
       ((<$>), (<|>), Alternative, liftA2, liftA)
import Data.Conduit as C
       ((=$), ($$), ($=))
import qualified Data.Conduit as C
import qualified Data.Conduit.Text as CT (decode, utf8)
import qualified Data.Conduit.List as CL
       (consume, concatMap, concatMapAccumM, sequence)
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Attoparsec (sinkParser)
import qualified Data.Attoparsec.Text as AP
       (endOfInput, takeWhile, satisfy, skipWhile, string, Parser,
        endOfLine, digit, manyTill, takeWhile1, char)
import Data.Attoparsec.Text ((<?>))
import Data.Char (isDigit)
import Data.Text (replace, Text)
import Data.Monoid ((<>))
import Data.Text.IO (hPutStrLn)

data ToolOutput = ToolInput Text
                | ToolError Text
                | ToolOutput Text
                | ToolPrompt Text
                | ToolExit ExitCode deriving(Eq, Show)

instance NFData ExitCode where
    rnf ExitSuccess = rnf ()
    rnf (ExitFailure failureCode) = rnf failureCode

instance  NFData ToolOutput where
    rnf (ToolInput s) = rnf s
    rnf (ToolError s) = rnf s
    rnf (ToolOutput s) = rnf s
    rnf (ToolPrompt s) = rnf s
    rnf (ToolExit code) = rnf code

data ToolCommand = ToolCommand Text Text (C.Sink ToolOutput IO ())
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
                   | ToolErrClosed deriving(Eq, Show)

toolline :: ToolOutput -> Text
toolline (ToolInput l)  = l
toolline (ToolOutput l) = l
toolline (ToolError l)  = l
toolline (ToolPrompt l)  = l
toolline (ToolExit _code) = ""

quoteArg :: Text -> Text
quoteArg s | T.any (==' ') s = "\"" <> (escapeQuotes s) <> "\""
quoteArg s                   = s

escapeQuotes :: Text -> Text
escapeQuotes = replace "\"" "\\\""

#ifdef MIN_VERSION_process_leksah
interruptProcessGroupOf :: ProcessHandle -> IO ()
interruptProcessGroupOf = interruptProcessGroup
#endif

runTool' :: FilePath -> [String] -> Maybe FilePath -> IO ([ToolOutput], ProcessHandle)
runTool' fp args mbDir = do
    debugM "leksah-server" $ "Start: " ++ show (fp, args)
    (out,pid) <- runTool fp args mbDir
    output <- C.runResourceT $ out $$ CL.consume
    waitForProcess pid
    debugM "leksah-server" $ "End: " ++ show (fp, args)
    return (output,pid)

runTool :: MonadIO m => FilePath -> [String] -> Maybe FilePath -> IO (C.Source m ToolOutput, ProcessHandle)
runTool executable arguments mbDir = do
    (Just inp,Just out,Just err,pid) <- createProcess (proc executable arguments)
        { std_in  = CreatePipe,
          std_out = CreatePipe,
          std_err = CreatePipe,
          cwd = mbDir,
#ifdef MIN_VERSION_process_leksah
          new_group = True }
#else
          create_group = True }
#endif
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

isolateToFirst p = loop
      where
        loop = C.await >>= maybe (return ()) (\x -> C.yield x >> when (p x) loop)

runInteractiveTool ::
    ToolState ->
    CommandLineReader ->
    FilePath ->
    [String] ->
    Maybe FilePath ->
    IO ()
runInteractiveTool tool clr executable arguments mbDir = do
    (Just inp,Just out,Just err,pid) <- createProcess (proc executable arguments)
        { std_in  = CreatePipe,
          std_out = CreatePipe,
          std_err = CreatePipe,
          cwd = mbDir,
#ifdef MIN_VERSION_process_leksah
          new_group = True }
#else
          create_group = True }
#endif
    putMVar (toolProcessMVar tool) pid
    output <- getOutput clr inp out err pid

    forkIO $ do
        commands <- getChanContents (toolCommandsRead tool)
        output $= outputSequence inp $$ processCommand commands inp
        return ()
    return ()
  where
    isEndOfCommandOutput (ToolPrompt _) = True
    isEndOfCommandOutput (ToolExit _) = True
    isEndOfCommandOutput _ = False

    isolateCommandOutput = isolateToFirst (not . isEndOfCommandOutput)

    processCommand [] _ = do
        liftIO $ debugM "leksah-server" $ "No More Commands"
        return ()
    processCommand ((command@(ToolCommand commandString rawCommandString handler)):remainingCommands) inp = do
        liftIO $ do
            debugM "leksah-server" $ "Command " ++ T.unpack commandString
            putMVar (currentToolCommand tool) command
            hPutStrLn inp commandString
            hFlush inp
        (mapM (C.yield . ToolInput) (T.lines rawCommandString) >> isolateCommandOutput) =$ handler
        processCommand remainingCommands inp

    outputSequence :: Handle -> C.Conduit RawToolOutput IO ToolOutput
    outputSequence inp =
        CL.concatMapAccumM writeCommandOutput (False, False, (outputSyncCommand clr), 0, "")
      where
        writeCommandOutput (RawToolOutput (ToolPrompt line)) (False, False, (Just outSyncCmd), n, _) = do
            debugM "leksah-server" $ "Pre Sync Prompt"
            hPutStrLn inp $ outSyncCmd n
            hFlush inp
            return ((True, False, (Just outSyncCmd), n, line), [])
        writeCommandOutput (RawToolOutput (ToolPrompt _))(True, False, mbSyncCmd, n, promptLine) = do
            debugM "leksah-server" $ "Unsynced Prompt"
            return ((True, False, mbSyncCmd, n, promptLine), [])
        writeCommandOutput (RawToolOutput o@(ToolOutput line)) (True, False, mbSyncCmd, n, promptLine) = do
            let synced = (isExpectedOutput clr n line)
            when synced $ debugM "leksah-server" $ "Output Sync Found"
            return ((True, synced, mbSyncCmd, n, promptLine), if synced then [] else [o])
        writeCommandOutput (RawToolOutput (ToolPrompt _)) (_, _, mbSyncCmd, n, promptLine) = do
            debugM "leksah-server" $ "Synced Prompt - Ready For Next Command"
            tryTakeMVar (currentToolCommand tool)
            return ((False, False, mbSyncCmd, n+1, promptLine), [ToolPrompt promptLine])
        writeCommandOutput (RawToolOutput o@(ToolExit _)) s = do
            debugM "leksah-server" $ "Tool Exit"
            putMVar (outputClosed tool) True
            return (s, [o])
        writeCommandOutput (RawToolOutput o) s = do
            return (s, [o])
        writeCommandOutput x s = do
            debugM "leksah-server" $ "Unexpected output " ++ show x
            return (s, [])

{-
newInteractiveTool :: (Handle -> Handle -> Handle -> ProcessHandle -> IO [RawToolOutput]) -> FilePath -> [String] -> IO ToolState
newInteractiveTool getOutput' executable arguments = do
    tool <- newToolState
    runInteractiveTool tool getOutput' executable arguments
    return tool
-}

ghciPrompt :: Text
ghciPrompt = "3KM2KWR7LZZbHdXfHUOA5YBBsJVYoCQnKX"

data CommandLineReader = CommandLineReader {
    parseInitialPrompt :: AP.Parser Text,
    parseFollowingPrompt :: AP.Parser Text,
    errorSyncCommand :: Maybe (Int -> Text),
    parseExpectedError :: AP.Parser (Text, Int),
    outputSyncCommand :: Maybe (Int -> Text),
    isExpectedOutput :: Int -> Text -> Bool
    }

ghciParseInitialPrompt :: AP.Parser Text
ghciParseInitialPrompt = (do
        ((AP.string "Prelude") <|> (AP.string "*"))
        AP.skipWhile (\c -> c /= '>' && c/= '\n')
        AP.string "> "
        return "")
    <?> "ghciParseInitialPrompt"

ghciParseFollowingPrompt :: AP.Parser Text
ghciParseFollowingPrompt = (do
        T.pack <$> AP.satisfy (/='\n') `AP.manyTill` (AP.string ghciPrompt))
    <?> "ghciParseFollowingPrompt"

marker :: Int -> Text
marker n = "kMAKWRALZZbHdXfHUOAAYBB" <> (T.pack $ show n)

parseMarker :: AP.Parser Int
parseMarker = (do
        AP.string $ T.pack "kMAKWRALZZbHdXfHUOAAYBB"
        nums <- AP.takeWhile isDigit
        return . read $ T.unpack nums)
    <?> "parseMarker"

ghciParseExpectedErrorCols :: AP.Parser ()
ghciParseExpectedErrorCols = (do
        AP.string $ T.pack "0-"
        AP.digit
        AP.digit
        return ())
    <|> (do
        AP.string $ T.pack "1-"
        AP.digit
        AP.digit
        return ())
    <|> (do
        AP.string $ T.pack "0"
        return ())
    <|> (do
        AP.string $ T.pack "1"
        return ())
    <?> "ghciParseExpectedErrorCols"

manyTill' :: Alternative f => f a -> f b -> f ([a], b)
manyTill' p end = scan
    where scan = liftA (\b -> ([], b)) end <|> liftA2 (\a (as, b) -> (a:as, b)) p scan

ghciParseExpectedError :: AP.Parser (Text, Int)
ghciParseExpectedError = (do
      (\(a, b) -> (T.pack a, b)) <$> AP.satisfy (/='\n') `manyTill'` (do
        AP.string "\n<interactive>:"
        AP.takeWhile1 isDigit
        AP.string ":"
        ghciParseExpectedErrorCols
        AP.string ": Not in scope: "
        (AP.char '`' <|> AP.char '‛')
        result <- parseMarker
        (AP.char '\'' <|> AP.char '’')
        AP.string "\n"
        return result))
    <?> "ghciParseExpectedError"

ghciIsExpectedOutput :: Int -> Text -> Bool
ghciIsExpectedOutput n =
    (==) (marker n)

ghciCommandLineReader :: CommandLineReader
ghciCommandLineReader    = CommandLineReader {
    parseInitialPrompt   = ghciParseInitialPrompt,
    parseFollowingPrompt = ghciParseFollowingPrompt,
    errorSyncCommand     = Just $ \n -> marker n,
    parseExpectedError   = ghciParseExpectedError,
    outputSyncCommand    = Just $ \n -> ":set prompt \"" <> marker n <> "\\n\"\n:set prompt " <> ghciPrompt,
    isExpectedOutput     = ghciIsExpectedOutput
    }

noInputCommandLineReader :: CommandLineReader
noInputCommandLineReader = CommandLineReader {
    parseInitialPrompt = fail "No Prompt Expected",
    parseFollowingPrompt = fail "No Prompt Expected",
    errorSyncCommand = Nothing,
    parseExpectedError = fail "No Expected Errors",
    outputSyncCommand = Nothing,
    isExpectedOutput = \_ _ -> False
    }

parseError :: AP.Parser (Text, Int) -> AP.Parser (Either (Text, Int) Text)
parseError expectedErrorParser = (do
        expected <- expectedErrorParser
        return $ Left expected)
    <|> (do
        line <- AP.takeWhile (/= '\n')
        (AP.endOfInput <|> AP.endOfLine)
        return $ Right line)
    <?> "parseError"

getOutput :: MonadIO m => CommandLineReader -> Handle -> Handle -> Handle -> ProcessHandle
    -> IO (C.Source m RawToolOutput)
getOutput clr inp out err pid = do
    hSetBuffering out NoBuffering
    hSetBuffering err NoBuffering
    mvar <- newEmptyMVar
    foundExpectedError <- liftIO $ newEmptyMVar
    forkIO $ do
        readError mvar err foundExpectedError
        putMVar mvar ToolErrClosed
    forkIO $ do
        readOutput mvar out foundExpectedError
        putMVar mvar ToolOutClosed
    return $ enumOutput mvar
  where
    enumOutput :: MonadIO m => MVar RawToolOutput -> C.Source m RawToolOutput
    enumOutput mvar = loop (0::Int) where
        loop closed | closed < 2 = do
            v <- liftIO $ takeMVar mvar
            nowClosed <- if (v == ToolOutClosed) || (v == ToolErrClosed)
                then return (closed + 1)
                else C.yield v >> return closed
            if nowClosed == 2
                then (liftIO $ waitForProcess pid) >>= (C.yield . RawToolOutput . ToolExit)
                else loop nowClosed
        loop _ = error "Error in enumOutput"

    readError :: MVar RawToolOutput -> Handle -> MVar Int -> IO ()
    readError mvar errors foundExpectedError = do
        CB.sourceHandle errors $= CT.decode CT.utf8
                    $= (CL.sequence (sinkParser (parseError $ parseExpectedError clr)))
                    $$ sendErrors
        hClose errors
      where
        sendErrors = C.awaitForever $ \x -> liftIO $ do
                            debugM "leksah-server" $ show x
                            case x of
                                Left (line, counter) -> do
                                    unless (T.null line) $ putMVar mvar $ RawToolOutput $ ToolError line
                                    putMVar foundExpectedError counter
                                Right line   -> putMVar mvar $ RawToolOutput $ ToolError line

    outputSequence :: AP.Parser ToolOutput -> AP.Parser ToolOutput -> C.Conduit Text IO ToolOutput
    outputSequence i1 i2 = loop
      where
        loop = C.await >>= maybe (return ()) (\x -> C.leftover x >> (sinkParser i1) >>= check)
        check line@(ToolPrompt _) = C.yield line >> CL.sequence (sinkParser i2)
        check line = C.yield line >> loop

    readOutput :: MVar RawToolOutput -> Handle -> MVar Int -> IO ()
    readOutput mvar output foundExpectedError = do
        let parseLines parsePrompt = ((do
                    lineSoFar <- parsePrompt
                    return $ ToolPrompt lineSoFar)
                <|> (do
                    line <- AP.takeWhile (/= '\n')
                    (AP.endOfInput <|> AP.endOfLine)
                    return $ ToolOutput line)
                <?> "parseLines")
            parseInitialLines = parseLines (parseInitialPrompt clr)
            parseFollowinglines = parseLines (parseFollowingPrompt clr)
        CB.sourceHandle output $= CT.decode CT.utf8
                    $= (outputSequence (parseInitialLines) (parseFollowinglines))
                    $$ sendErrors
        hClose output
      where
        sendErrors = loop 0 False ""
            where
                loop counter errSynced promptLine = do
                    mbx <- C.await
                    liftIO $ debugM "leksah-server" $ "sendErrors " ++ show mbx
                    case mbx of
                        Nothing -> return ()
                        Just x@(ToolPrompt line) -> do
                            case (counter, errSynced, errorSyncCommand clr) of
                                (0, _, _) -> do
                                    loop (counter+1) errSynced line
                                (_, False, Just syncCmd) -> do
                                    liftIO $ do
                                        debugM "leksah-server" $ "sendErrors - Sync " ++ (T.unpack $ syncCmd counter)
                                        hPutStrLn inp $ syncCmd counter
                                        hFlush inp
                                        waitForError counter
                                        debugM "leksah-server" $ "sendErrors - Synced " ++ show counter
                                    loop (counter+1) True line
                                (_, True, Just _) -> do
                                    liftIO $ putMVar mvar $ RawToolOutput (ToolPrompt promptLine)
                                    loop (counter+1) False promptLine
                                _ -> do
                                    liftIO $ putMVar mvar $ RawToolOutput x
                                    loop (counter+1) False promptLine
                        Just x -> do
                            liftIO . putMVar mvar $ RawToolOutput x
                            loop counter errSynced promptLine

        waitForError counter = do
            foundCount <- takeMVar foundExpectedError
            debugM "leksah-server" $ "waitForError - Found " ++ show foundCount
            when (foundCount < counter) $ waitForError counter


fromRawOutput :: RawToolOutput -> [ToolOutput]
fromRawOutput (RawToolOutput output) = [output]
fromRawOutput (_) = []

getOutputNoPrompt :: MonadIO m => Handle -> Handle -> Handle -> ProcessHandle -> IO (C.Source m ToolOutput)
getOutputNoPrompt inp out err pid = do
    output <- getOutput noInputCommandLineReader inp out err pid
    return $ output $= CL.concatMap fromRawOutput

newGhci' :: [String] -> (C.Sink ToolOutput IO ()) -> IO ToolState
newGhci' flags startupOutputHandler = do
    tool <- newToolState
    writeChan (toolCommands tool) $
        ToolCommand (":set prompt " <> ghciPrompt) "" startupOutputHandler
    runInteractiveTool tool ghciCommandLineReader "ghci" flags Nothing
    return tool

newGhci :: FilePath -> Maybe String -> [String] -> (C.Sink ToolOutput IO ()) -> IO ToolState
newGhci dir mbExe interactiveFlags startupOutputHandler = do
        tool <- newToolState
        writeChan (toolCommands tool) $
            ToolCommand (":set " <> T.pack (unwords interactiveFlags) <> "\n:set prompt " <> ghciPrompt) "" startupOutputHandler
        runInteractiveTool tool ghciCommandLineReader "cabal"
            ("repl" : maybeToList mbExe) (Just dir)
        return tool

executeCommand :: ToolState -> String -> String -> C.Sink ToolOutput IO () -> IO ()
executeCommand tool command rawCommand handler = do
    writeChan (toolCommands tool) $ ToolCommand (T.pack command) (T.pack rawCommand) handler

executeGhciCommand :: ToolState -> String -> C.Sink ToolOutput IO () -> IO ()
executeGhciCommand tool command handler = do
    if '\n' `elem` command
        then executeCommand tool safeCommand command handler
        else executeCommand tool command command handler
    where
        filteredLines = (filter safeLine (lines command))
        safeCommand = ":cmd (return " ++ show (":{\n" ++ unlines filteredLines ++ "\n:}") ++ ")"
        safeLine ":{" = False
        safeLine ":}" = False
        safeLine _ = True

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

