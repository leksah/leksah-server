{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
    isToolPrompt,
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
    interruptTool,
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

import Control.Applicative
import Prelude
import Control.Concurrent
       (tryPutMVar, tryTakeMVar, readMVar, takeMVar, putMVar,
        newEmptyMVar, forkIO, newChan, MVar, Chan, writeChan,
        getChanContents, dupChan)
import Control.Monad (forM_, void, forever, when, unless)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Maybe (fromMaybe)
import System.Process
       (showCommandForUser, proc, waitForProcess, ProcessHandle,
        createProcess, CreateProcess(..), interruptProcessGroupOf,
        runCommand, getProcessExitCode, runProcess, runInteractiveProcess,
        readProcessWithExitCode, terminateProcess)
import System.Process.Internals (StdStream(..))
import qualified Data.Text as T
       (unlines, unwords, null, lines, any, unpack, pack, filter)
import Control.DeepSeq
import System.Log.Logger (debugM)
import System.Exit (ExitCode(..))
import System.IO
       (hClose, hFlush, Handle, hSetBuffering, BufferMode(..))
import System.Directory (getHomeDirectory)
import System.FilePath (dropTrailingPathSeparator)
import System.Timeout (timeout)
import Data.Conduit as C
       (fuseUpstream, runConduit, ($$), ($=))
import qualified Data.Conduit as C
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Conduit.Text as CT (decode, utf8)
import qualified Data.Conduit.List as CL
       (consume, sequence, map)
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Attoparsec (sinkParser)
import qualified Data.Attoparsec.Text as AP
       (endOfInput, takeWhile, Parser, endOfLine, char)
import Data.Attoparsec.Text ((<?>))
import Data.Text (replace, Text)
import Data.Monoid ((<>))
import Data.Text.IO (hPutStrLn)
import Data.List (stripPrefix)
#ifdef MIN_VERSION_unix
import System.Posix.Signals
       (emptySignalSet, sigINT, addSignal, unblockSignals)
import qualified Data.Map as M (toList)
import Data.Map (Map)
#endif

data ToolOutput = ToolInput Text
                | ToolError Text
                | ToolOutput Text
                | ToolPrompt Text
                | ToolExit ExitCode deriving(Eq, Show)

#if !MIN_VERSION_process(1,4,0)
instance NFData ExitCode where
    rnf ExitSuccess = rnf ()
    rnf (ExitFailure failureCode) = rnf failureCode
#endif

instance  NFData ToolOutput where
    rnf (ToolInput s) = rnf s
    rnf (ToolError s) = rnf s
    rnf (ToolOutput s) = rnf s
    rnf (ToolPrompt s) = rnf s
    rnf (ToolExit code) = rnf code

isEndOfCommandOutput :: ToolOutput -> Bool
isEndOfCommandOutput (ToolPrompt _) = True
isEndOfCommandOutput (ToolExit _) = True
isEndOfCommandOutput _ = False

data ToolCommand = ToolCommand Text Text (C.Sink ToolOutput IO ())
data ToolState = ToolState {
    toolProcessMVar :: MVar ProcessHandle,
    outputClosed :: MVar Bool,
    toolCommands :: Chan ToolCommand,
    toolCommandsRead :: Chan ToolCommand,
    currentToolCommand :: MVar Text,
    interruptToolMVar :: MVar ()}

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

isToolPrompt :: ToolOutput -> Bool
isToolPrompt (ToolPrompt _) = True
isToolPrompt _ = False

quoteArg :: Text -> Text
quoteArg s | T.any (==' ') s = "\"" <> escapeQuotes s <> "\""
quoteArg s                   = s

escapeQuotes :: Text -> Text
escapeQuotes = replace "\"" "\\\""

#ifdef MIN_VERSION_process_leksah
interruptProcessGroupOf :: ProcessHandle -> IO ()
interruptProcessGroupOf = interruptProcessGroup
#endif

runTool' :: FilePath -> [Text] -> Maybe FilePath -> Maybe [(String,String)] -> IO ([ToolOutput], ProcessHandle)
runTool' fp args mbDir mbEnv = do
    debugM "leksah-server" $ "Start: " ++ show (fp, args)
    (out,pid) <- runTool fp args mbDir mbEnv
    output <- runResourceT $ out $$ CL.consume
    _ <- waitForProcess pid
    debugM "leksah-server" $ "End: " ++ show (fp, args)
    return (output,pid)

runTool :: MonadIO m => FilePath -> [Text] -> Maybe FilePath -> Maybe [(String,String)] -> IO (C.Source m ToolOutput, ProcessHandle)
runTool executable arguments mbDir mbEnv = do
#ifdef MIN_VERSION_unix
    -- As of GHC 7.10.1 both createProcess and the GHC GC use
    unblockSignals $ addSignal sigINT emptySignalSet
#endif
    (Just inp,Just out,Just err,pid) <- createProcess (proc executable (map T.unpack arguments))
        { std_in  = CreatePipe,
          std_out = CreatePipe,
          std_err = CreatePipe,
          cwd = mbDir,
          env = mbEnv,
#ifdef MIN_VERSION_process_leksah
          new_group = True }
#else
          create_group = True }
#endif
    output <- getOutputNoPrompt inp out err pid
    return (do
        home <- liftIO getHomeDirectory
        let friendlyDir d = case stripPrefix home d of
                                Just rest -> "~" <> rest
                                Nothing -> d
        C.yield . ToolInput . ((fromMaybe "" (T.pack . friendlyDir . dropTrailingPathSeparator <$> mbDir) <> "$ ") <>) . T.pack $ showCommandForUser executable (map T.unpack arguments)
        output, pid)

newToolState :: IO ToolState
newToolState = do
    toolProcessMVar <- newEmptyMVar
    outputClosed <- newEmptyMVar
    toolCommands <- newChan
    toolCommandsRead <- dupChan toolCommands
    currentToolCommand <- newEmptyMVar
    interruptToolMVar <- newEmptyMVar
    return ToolState{..}

runInteractiveTool ::
    ToolState ->
    CommandLineReader ->
    FilePath ->
    [Text] ->
    Maybe FilePath ->
    Maybe (Map String String) ->
    C.Sink ToolOutput IO () ->
    IO ()
runInteractiveTool tool clr executable arguments mbDir mbEnv idleOutput = do
    (Just inp,Just out,Just err,pid) <- createProcess (proc executable (map T.unpack arguments))
        { std_in  = CreatePipe,
          std_out = CreatePipe,
          std_err = CreatePipe,
          cwd = mbDir,
          env = M.toList <$> mbEnv,
#ifdef MIN_VERSION_process_leksah
          new_group = True }
#else
          create_group = True }
#endif
    putMVar (toolProcessMVar tool) pid
    rawOutput <- getOutput clr inp out err pid
    _ <- forkIO . forever $ do
        takeMVar $ interruptToolMVar tool
        interruptProcessGroupOf pid

    _ <- forkIO $ do
        case initialCommand clr of
            Just cmd -> hPutStrLn inp cmd >> hFlush inp
            Nothing  -> return ()
        commands <- getChanContents (toolCommandsRead tool)
        output <- newEmptyMVar
        cmd <- newEmptyMVar
        _ <- forkIO $ outputSequence inp rawOutput output
        _ <- forkIO $ forM_ commands $ putMVar cmd
        processNoCommand inp output cmd $$ idleOutput
        return ()
    return ()
  where

    processNoCommand inp output cmd = loop
      where
        loop = do
            liftIO $ do
                ready <- newEmptyMVar
                _ <- forkIO $ readMVar output >> putMVar ready ()
                _ <- forkIO $ readMVar cmd >> putMVar ready ()
                takeMVar ready
            liftIO (tryTakeMVar output) >>= \case
                Just o -> step o
                Nothing -> liftIO (takeMVar cmd) >>= processCommand inp output cmd
        step o@(ToolExit _) = do
            liftIO . debugM "leksah-server" $ "Idle output " <> show o
            C.yield o
        step o = do
            liftIO . debugM "leksah-server" $ "Idle output " <> show o
            C.yield o
            loop

    processCommand inp output cmd (ToolCommand commandString rawCommandString handler) = do
        liftIO $ do
            debugM "leksah-server" $ "Command " ++ T.unpack commandString
            putMVar (currentToolCommand tool) commandString
            hPutStrLn inp commandString
            hFlush inp
        liftIO (runConduit $ fuseUpstream (do
                mapM_ (C.yield . ToolInput) (T.lines rawCommandString)
                loop) handler) >>= \case
            ToolExit _ -> return ()
            _ -> processNoCommand inp output cmd
      where
        loop = do
            o <- liftIO (takeMVar output)
            C.yield o
            if isEndOfCommandOutput o
                then return o
                else loop

    outputSequence :: Handle -> MVar RawToolOutput -> MVar ToolOutput -> IO ()
    outputSequence inp rawOut output = loop (False, False, outputSyncCommand clr, 0, "")
      where
        loop :: (Bool, Bool, Maybe (Int -> Text), Int, Text) -> IO ()
        loop s = takeMVar rawOut >>= (`writeCommandOutput` s)
        yieldOutput = putMVar output
        writeCommandOutput (RawToolOutput (ToolPrompt line)) (False, False, Just outSyncCmd, n, _) = do
            debugM "leksah-server" "Pre Sync Prompt"
            hPutStrLn inp $ outSyncCmd n
            hFlush inp
            loop (True, False, Just outSyncCmd, n, line)
        writeCommandOutput (RawToolOutput (ToolPrompt _))(True, False, mbSyncCmd, n, promptLine) = do
            debugM "leksah-server" "Unsynced Prompt"
            loop (True, False, mbSyncCmd, n, promptLine)
        writeCommandOutput (RawToolOutput o@(ToolOutput line)) (True, False, mbSyncCmd, n, promptLine) = do
            let synced = isExpectedOutput clr n line
            when synced $ debugM "leksah-server" "Output Sync Found"
            unless synced $ yieldOutput o
            loop (True, synced, mbSyncCmd, n, promptLine)
        writeCommandOutput (RawToolOutput (ToolPrompt _)) (_, _, mbSyncCmd, n, promptLine) = do
            debugM "leksah-server" "Synced Prompt - Ready For Next Command"
            _ <- tryTakeMVar (currentToolCommand tool)
            yieldOutput $ ToolPrompt promptLine
            loop (False, False, mbSyncCmd, n+1, promptLine)
        writeCommandOutput (RawToolOutput o@(ToolExit _)) _ = do
            debugM "leksah-server" "Tool Exit"
            putMVar (outputClosed tool) True
            yieldOutput o
        writeCommandOutput (RawToolOutput o) s = do
            yieldOutput o
            loop s
        writeCommandOutput x s = do
            debugM "leksah-server" $ "Unexpected output " ++ show x
            loop s

{-
newInteractiveTool :: (Handle -> Handle -> Handle -> ProcessHandle -> IO [RawToolOutput]) -> FilePath -> [Text] -> IO ToolState
newInteractiveTool getOutput' executable arguments = do
    tool <- newToolState
    runInteractiveTool tool getOutput' executable arguments
    return tool
-}

ghciPrompt :: Text
ghciPrompt = "\0"

data CommandLineReader = CommandLineReader {
    initialCommand :: Maybe Text,
    parseInitialPrompt :: AP.Parser Text,
    parseFollowingPrompt :: AP.Parser Text,
    errorSyncCommand :: Maybe Text,
    parseExpectedError :: AP.Parser Text,
    outputSyncCommand :: Maybe (Int -> Text),
    isExpectedOutput :: Int -> Text -> Bool
    }

ghciParsePrompt :: AP.Parser Text
ghciParsePrompt = (do
        t <- AP.takeWhile (\ c -> c /= '\n' && c /= '\0')
        _ <- AP.char '\0'
        return t)
    <?> "ghciParsePrompt"

marker :: Int -> Text
marker n = "kMAKWRALZ" <> T.pack (show n)

ghciIsExpectedOutput :: Int -> Text -> Bool
ghciIsExpectedOutput n =
    (==) (marker n)

ghciCommandLineReader :: CommandLineReader
ghciCommandLineReader    = CommandLineReader {
    initialCommand       = Just $ ":set prompt " <> ghciPrompt,
    parseInitialPrompt   = ghciParsePrompt,
    parseFollowingPrompt = ghciParsePrompt,
    errorSyncCommand     = Just "_ <- System.IO.hPutStr System.IO.stderr \"\\0\"",
    parseExpectedError   = ghciParsePrompt,
    outputSyncCommand    = Just $ \count -> ":set prompt \"" <> marker count <> "\\n\"\n:set prompt " <> ghciPrompt,
    isExpectedOutput     = ghciIsExpectedOutput
    }

noInputCommandLineReader :: CommandLineReader
noInputCommandLineReader = CommandLineReader {
    initialCommand = Nothing,
    parseInitialPrompt = fail "No Prompt Expected",
    parseFollowingPrompt = fail "No Prompt Expected",
    errorSyncCommand = Nothing,
    parseExpectedError = fail "No Expected Errors",
    outputSyncCommand = Nothing,
    isExpectedOutput = \_ _ -> False
    }

parseError :: AP.Parser Text -> AP.Parser (Either Text Text)
parseError expectedErrorParser = (do
        expected <- expectedErrorParser
        return $ Left expected)
    <|> (do
        line <- AP.takeWhile (/= '\n')
        AP.endOfInput <|> AP.endOfLine
        return $ Right line)
    <?> "parseError"

getOutput :: MonadIO m => CommandLineReader -> Handle -> Handle -> Handle -> ProcessHandle
    -> m (MVar RawToolOutput)
getOutput clr inp out err pid = liftIO $ do
    hSetBuffering out NoBuffering
    hSetBuffering err NoBuffering
    mvar <- newEmptyMVar
    foundExpectedError <- liftIO newEmptyMVar
    _ <- forkIO $ do
        readError mvar err foundExpectedError
        putMVar mvar ToolErrClosed
    _ <- forkIO $ do
        readOutput mvar out foundExpectedError
        putMVar mvar ToolOutClosed
    resultMVar <- newEmptyMVar
    _ <- forkIO $ checkForExit mvar resultMVar
    return resultMVar
  where
    checkForExit :: MVar RawToolOutput -> MVar RawToolOutput -> IO ()
    checkForExit mvar resultMVar = loop (0:: Int) where
        loop closed | closed < 2 = do
            v <- liftIO $ takeMVar mvar
            nowClosed <- if (v == ToolOutClosed) || (v == ToolErrClosed)
                then return (closed + 1)
                else putMVar resultMVar v >> return closed
            if nowClosed == 2
                then do
                    exitCode <- liftIO $ waitForProcess pid
                    liftIO $ hClose inp
                    putMVar resultMVar . RawToolOutput $ ToolExit exitCode
                else loop nowClosed
        loop _ = error "Error in checkForExit"

    readError :: MVar RawToolOutput -> Handle -> MVar () -> IO ()
    readError mvar errors foundExpectedError = do
        CB.sourceHandle errors $= CT.decode CT.utf8
                    $= CL.map (T.filter (/= '\r'))
--                    $= CL.map (\x -> trace ("E : " <> show x) x)
                    $= CL.sequence (sinkParser (parseError $ parseExpectedError clr))
                    $$ sendErrors
        hClose errors
      where
        sendErrors = C.awaitForever $ \x -> liftIO $ do
                            debugM "leksah-server" $ show x
                            case x of
                                Left line -> do
                                    unless (T.null line) $ putMVar mvar $ RawToolOutput $ ToolError line
                                    void $ tryPutMVar foundExpectedError ()
                                Right line   -> putMVar mvar $ RawToolOutput $ ToolError line

    outputSequence :: AP.Parser ToolOutput -> AP.Parser ToolOutput -> C.Conduit Text IO ToolOutput
    outputSequence i1 i2 = loop
      where
        loop = C.await >>= maybe (return ()) (\x -> C.leftover x >> sinkParser i1 >>= check)
        check line@(ToolPrompt _) = C.yield line >> CL.sequence (sinkParser i2)
        check line = C.yield line >> loop

    readOutput :: MVar RawToolOutput -> Handle -> MVar () -> IO ()
    readOutput mvar output foundExpectedError = do
        let parseLines parsePrompt = (do
                    lineSoFar <- parsePrompt
                    return $ ToolPrompt lineSoFar)
                <|> (do
                    line <- AP.takeWhile (/= '\n')
                    AP.endOfInput <|> AP.endOfLine
                    return $ ToolOutput line)
                <?> "parseLines"
            parseInitialLines = parseLines (parseInitialPrompt clr)
            parseFollowinglines = parseLines (parseFollowingPrompt clr)
        CB.sourceHandle output $= CT.decode CT.utf8
                    $= CL.map (T.filter (/= '\r'))
                    $= outputSequence parseInitialLines parseFollowinglines
                    $$ sendErrors
        hClose output
      where
        sendErrors = loop True False ""
            where
                loop first errSynced promptLine = do
                    mbx <- C.await
                    liftIO $ debugM "leksah-server" $ "sendErrors " ++ show mbx
                    case mbx of
                        Nothing -> return ()
                        Just x@(ToolPrompt line) ->
                            case (first, errSynced, errorSyncCommand clr) of
                                (True, _, _) ->
                                    loop False errSynced line
                                (_, False, Just syncCmd) -> do
                                    liftIO $ do
                                        debugM "leksah-server" $ "sendErrors - Sync " ++ T.unpack syncCmd
                                        hPutStrLn inp syncCmd
                                        hFlush inp
                                        _ <- timeout 10000000 waitForError
                                        debugM "leksah-server" "sendErrors - Synced"
                                    loop False True line
                                (_, True, Just _) -> do
                                    liftIO $ putMVar mvar $ RawToolOutput (ToolPrompt promptLine)
                                    loop False False promptLine
                                _ -> do
                                    liftIO $ putMVar mvar $ RawToolOutput x
                                    loop False False promptLine
                        Just x -> do
                            liftIO . putMVar mvar $ RawToolOutput x
                            loop first errSynced promptLine

        waitForError = takeMVar foundExpectedError


fromRawOutput :: RawToolOutput -> Maybe ToolOutput
fromRawOutput (RawToolOutput output) = Just output
fromRawOutput _ = Nothing

getOutputNoPrompt :: MonadIO m => Handle -> Handle -> Handle -> ProcessHandle -> IO (C.Source m ToolOutput)
getOutputNoPrompt inp out err pid = do
    output <- getOutput noInputCommandLineReader inp out err pid
    return $ loop output
  where
    loop output = do
        o <- liftIO $ fromRawOutput <$> takeMVar output
        mapM_ C.yield o
        unless (any isEndOfCommandOutput o) $ loop output

newGhci' :: [Text] -> C.Sink ToolOutput IO () -> C.Sink ToolOutput IO () -> IO ToolState
newGhci' flags startupOutputHandler idleOutputHandler = do
    tool <- newToolState
    writeChan (toolCommands tool) $
        ToolCommand (":set prompt " <> ghciPrompt) "" startupOutputHandler
    runInteractiveTool tool ghciCommandLineReader "ghci" flags Nothing Nothing idleOutputHandler
    return tool

newGhci :: FilePath -> [Text] -> FilePath -> Maybe (Map String String) -> [Text] -> C.Sink ToolOutput IO () -> C.Sink ToolOutput IO () -> IO ToolState
newGhci executable arguments dir mbEnv interactiveFlags startupOutputHandler idleOutputHandler = do
    tool <- newToolState
    home <- liftIO getHomeDirectory
    let friendlyDir d = case stripPrefix home d of
                            Just rest -> "~" <> rest
                            Nothing -> d
        startupCommand = friendlyDir (dropTrailingPathSeparator dir) <> "$ "
                         <> showCommandForUser executable (map T.unpack arguments)
    debugM "leksah-server" $ "newGhci " <> startupCommand

    writeChan (toolCommands tool) $
        ToolCommand (":set prompt " <> ghciPrompt) (T.pack startupCommand) startupOutputHandler
    executeGhciCommand tool (":set " <> T.unwords interactiveFlags) startupOutputHandler
    runInteractiveTool tool ghciCommandLineReader executable arguments (Just dir) mbEnv idleOutputHandler
    return tool

executeCommand :: ToolState -> Text -> Text -> C.Sink ToolOutput IO () -> IO ()
executeCommand tool command rawCommand handler =
    writeChan (toolCommands tool) $ ToolCommand command rawCommand handler

executeGhciCommand :: ToolState -> Text -> C.Sink ToolOutput IO () -> IO ()
executeGhciCommand tool command handler =
    if '\n' `elem` T.unpack command
        then executeCommand tool safeCommand command handler
        else executeCommand tool command command handler
    where
        filteredLines = filter safeLine (T.lines command)
        safeCommand = ":cmd (return " <> T.pack (show $ ":{\n" <> T.unlines filteredLines <> "\n:}") <> ")"
        safeLine ":{" = False
        safeLine ":}" = False
        safeLine _ = True

interruptTool :: ToolState -> IO ()
interruptTool tool = void $ tryPutMVar (interruptToolMVar tool) ()

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

