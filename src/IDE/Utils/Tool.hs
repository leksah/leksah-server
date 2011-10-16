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
       (tryTakeMVar, readMVar, takeMVar, putMVar,
        newEmptyMVar, forkIO, newChan, MVar, Chan, writeChan,
        getChanContents, dupChan)
import Control.Monad (forM_, when)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.List (stripPrefix)
import Data.Maybe (catMaybes)
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
import System.Log.Logger (debugM)
import System.Exit (ExitCode(..))
import System.IO (hFlush, hPutStrLn, Handle)
import Control.Applicative ((<|>))
import Data.Enumerator.Binary (enumHandle)
import Data.Enumerator as E
       ((=$), (>>==), Stream(..), Enumeratee, Enumerator, run, ($$),
        ($=), (>==>))
import qualified Data.Enumerator.Binary as EB (filter)
import Data.Attoparsec.Enumerator (iterParser)
import qualified Data.Attoparsec as AP
       (endOfInput, word8, takeWhile, try, skipWhile, string, Parser)
import Data.ByteString (ByteString)
import qualified Data.Enumerator as E
       (enumList, returnI, Step(..), isEOF, checkDone, yield,
        continue, Iteratee(..), sequence, run_)
import qualified Data.Attoparsec.Char8 as AP (digit, isDigit_w8)
import qualified Data.ByteString.Char8 as B (unpack, pack)
import Data.Attoparsec ((<?>))
import qualified Data.Enumerator.List as EL
       (mapAccumM, filter, consume, concatMap, concatMapAccumM)

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

data ToolCommand = ToolCommand String (E.Iteratee ToolOutput IO ())
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
    output <- E.run_ $ out $$ EL.consume
    waitForProcess pid
    debugM "leksah-server" $ "End: " ++ show (fp, args)
    return (output,pid)

runTool :: MonadIO m => FilePath -> [String] -> Maybe FilePath -> IO (Enumerator ToolOutput m b, ProcessHandle)
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

dropToFirst :: Monad m => (a -> Bool) -> E.Iteratee a m ()
dropToFirst p = E.continue loop where
    loop (Chunks xs) = case dropWhile p xs of
        []    -> E.continue loop
        _:xs' -> E.yield () (Chunks xs')
    loop EOF = E.yield () EOF

isolateToFirst :: Monad m => (a -> Bool) -> Enumeratee a a m b
isolateToFirst p (E.Continue k) = E.continue loop where
    loop (Chunks []) = E.continue loop
    loop (Chunks xs) =
        case span p xs of
            (_, [])    -> k (Chunks xs) >>== isolateToFirst p
            (s1, t:s2) -> k (Chunks (s1++[t])) >>== (\step -> E.yield step (Chunks s2))
    loop EOF = k EOF >>== (\step -> E.yield step EOF)
isolateToFirst p step = dropToFirst p >> return step

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
#ifdef MIN_VERSION_process_leksah
          new_group = True }
#else
          create_group = True }
#endif
    putMVar (toolProcessMVar tool) pid
    output <- getOutput clr inp out err pid

    forkIO $ do
        commands <- getChanContents (toolCommandsRead tool)
        E.run_ $ output $$ outputSequence inp $$ processCommand commands inp
        return ()
    return ()
  where
    isEndOfCommandOutput ToolPrompt = True
    isEndOfCommandOutput (ToolExit _) = True
    isEndOfCommandOutput _ = False

    isolateCommandOutput = isolateToFirst (not . isEndOfCommandOutput)

    processCommand [] _ = do
        liftIO $ debugM "leksah-server" $ "No More Commands"
        return ()
    processCommand ((command@(ToolCommand commandString handler)):remainingCommands) inp = do
        liftIO $ putMVar (currentToolCommand tool) command
        liftIO $ hPutStrLn inp commandString
        liftIO $ hFlush inp

        (E.enumList 1 (map ToolInput (lines commandString)) >==> isolateCommandOutput) =$ handler
        processCommand remainingCommands inp

    outputSequence :: Handle -> E.Enumeratee RawToolOutput ToolOutput IO b
    outputSequence inp =
        EL.concatMapAccumM writeCommandOutput (False, False, (outputSyncCommand clr), 0)
      where
        writeCommandOutput (False, False, (Just outSyncCmd), n) (RawToolOutput ToolPrompt) = do
            debugM "leksah-server" $ "Pre Sync Prompt"
            hPutStrLn inp $ outSyncCmd n
            hFlush inp
            return ((True, False, (Just outSyncCmd), n), [])
        writeCommandOutput (True, False, mbSyncCmd, n) (RawToolOutput ToolPrompt) = do
            debugM "leksah-server" $ "Unsynced Prompt"
            return ((True, False, mbSyncCmd, n), [])
        writeCommandOutput (True, False, mbSyncCmd, n) (RawToolOutput o@(ToolOutput line)) = do
            let synced = (isExpectedOutput clr n line)
            when synced $ debugM "leksah-server" $ "Output Sync Found"
            return ((True, synced, mbSyncCmd, n), if synced then [] else [o])
        writeCommandOutput (_, _, mbSyncCmd, n) (RawToolOutput ToolPrompt) = do
            debugM "leksah-server" $ "Synced Prompt - Ready For Next Command"
            tryTakeMVar (currentToolCommand tool)
            return ((False, False, mbSyncCmd, n+1), [ToolPrompt])
        writeCommandOutput s (RawToolOutput o@(ToolExit _)) = do
            debugM "leksah-server" $ "Tool Exit"
            putMVar (outputClosed tool) True
            return (s, [o])
        writeCommandOutput s (RawToolOutput o) = do
            return (s, [o])
        writeCommandOutput s _ = do
            return (s, [])

{-
newInteractiveTool :: (Handle -> Handle -> Handle -> ProcessHandle -> IO [RawToolOutput]) -> FilePath -> [String] -> IO ToolState
newInteractiveTool getOutput' executable arguments = do
    tool <- newToolState
    runInteractiveTool tool getOutput' executable arguments
    return tool
-}

ghciPrompt :: ByteString
ghciPrompt = B.pack "3KM2KWR7LZZbHdXfHUOA5YBBsJVYoCQnKX"

data CommandLineReader = CommandLineReader {
    parseInitialPrompt :: AP.Parser (),
    parseFollowingPrompt :: AP.Parser (),
    errorSyncCommand :: Maybe (Int -> String),
    parseExpectedError :: AP.Parser Int,
    outputSyncCommand :: Maybe (Int -> String),
    isExpectedOutput :: Int -> String -> Bool
    }

ghciParseInitialPrompt :: AP.Parser ()
ghciParseInitialPrompt = do
    ((AP.string $ B.pack "Prelude") <|> (AP.string $ B.pack "*"))
    AP.skipWhile (/= 62) -- 62 == ord '>'
    AP.string $ B.pack "> "
    return ()

ghciParseFollowingPrompt :: AP.Parser ()
ghciParseFollowingPrompt = do
    AP.string $ ghciPrompt
    return ()

{-
stripMarker $ marker 0 ++ "dfskfjdkl"
-}

marker :: Int -> String
marker n = "kMAKWRALZZbHdXfHUOAAYBB" ++ show n

parseMarker :: AP.Parser Int
parseMarker = AP.try (do
        AP.string $ B.pack "kMAKWRALZZbHdXfHUOAAYBB"
        nums <- AP.takeWhile AP.isDigit_w8
        return . read $ B.unpack nums)
    <?> "parseMarker"

ghciParseExpectedErrorCols :: AP.Parser ()
ghciParseExpectedErrorCols = AP.try (do
        AP.string $ B.pack "0-"
        AP.digit
        AP.digit
        return ())
    <|> AP.try (do
        AP.string $ B.pack "1-"
        AP.digit
        AP.digit
        return ())
    <|> AP.try (do
        AP.string $ B.pack "0"
        return ())
    <|> AP.try (do
        AP.string $ B.pack "1"
        return ())
    <?> "ghciParseExpectedErrorCols"

ghciParseExpectedError :: AP.Parser Int
ghciParseExpectedError = AP.try (do
        AP.string $ B.pack "\n<interactive>:1:"
        ghciParseExpectedErrorCols
        AP.string $ B.pack ": Not in scope: `"
        result <- parseMarker
        AP.string $ B.pack "'\n"
        return result)
    <?> "ghciParseExpectedError"

ghciIsExpectedOutput :: Int -> String -> Bool
ghciIsExpectedOutput n =
    (==) (marker n)

ghciCommandLineReader :: CommandLineReader
ghciCommandLineReader    = CommandLineReader {
    parseInitialPrompt   = ghciParseInitialPrompt,
    parseFollowingPrompt = ghciParseFollowingPrompt,
    errorSyncCommand     = Just $ \n -> marker n,
    parseExpectedError   = ghciParseExpectedError,
    outputSyncCommand    = Just $ \n -> ":set prompt \"" ++ marker n ++ "\\n\"\n:set prompt " ++ B.unpack ghciPrompt,
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

parseError :: AP.Parser Int -> AP.Parser (Either Int ByteString)
parseError expectedErrorParser = AP.try (do
        counter <- expectedErrorParser
        return $ Left counter)
    <|> AP.try (do
        line <- AP.takeWhile (/= 10)
        (AP.word8 10 >> return () <|> AP.endOfInput)
        return $ Right line)
    <?> "parseError"

getOutput :: MonadIO m => CommandLineReader -> Handle -> Handle -> Handle -> ProcessHandle
    -> IO (Enumerator RawToolOutput m b)
getOutput clr inp out err pid = do
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
    enumOutput :: MonadIO m => MVar RawToolOutput -> Enumerator RawToolOutput m b
    enumOutput = loop (0::Int) where
        loop closed mvar (E.Continue k) | closed < 2 = do
            v <- liftIO $ takeMVar mvar
            let (nowClosed, chunk) = if (v == ToolOutClosed) || (v == ToolErrClosed)
                                        then (closed + 1, [])
                                        else (closed, [v])
            if nowClosed == 2
                then do
                    exitCode <- liftIO $ waitForProcess pid
                    k (Chunks [RawToolOutput (ToolExit exitCode)]) >>== loop nowClosed mvar
                else k (Chunks chunk) >>== loop nowClosed mvar
        loop _ _ step = E.returnI step

    readError :: MVar RawToolOutput -> Handle -> MVar Int -> IO ()
    readError mvar errors foundExpectedError = do
        result <- E.run $ (enumHandle 2048 errors $= EB.filter (/= 13))
                    $$ (E.sequence (iterParser $ parseError (parseExpectedError clr)))
                    $$ sendErrors
        case result of
            Left e  -> putStrLn $ show e
            Right _ -> return ()
        return ()
      where
        sendErrors = E.continue loop
            where
                loop (E.Chunks xs) = do
                    forM_ xs $ \x -> liftIO $ do
                        debugM "leksah-server" $ show x
                        case x of
                            Left counter -> putMVar foundExpectedError counter
                            Right line   -> putMVar mvar $ RawToolOutput $ ToolError (B.unpack line)
                    E.continue loop
                loop E.EOF = E.yield () E.EOF

    outputSequence :: E.Iteratee ao IO ToolOutput -> E.Iteratee ao IO ToolOutput -> E.Enumeratee ao ToolOutput IO b
    outputSequence i1 i2 = loop i1 where
        loop i = E.checkDone check
          where
            check k = E.isEOF >>= \f -> if f
                then E.yield (E.Continue k) EOF
                else step k
            step k = i >>= \v ->
                case v of
                    ToolPrompt -> k (Chunks [v]) >>== loop i2
                    _ -> k (Chunks [v]) >>== loop i

    readOutput :: MVar RawToolOutput -> Handle -> MVar Int -> IO ()
    readOutput mvar output foundExpectedError = do
        let parseLines parsePrompt = AP.try (do
                    parsePrompt
                    return ToolPrompt)
                <|> AP.try (do
                    line <- AP.takeWhile (/= 10)
                    ((AP.word8 10 >> return ()) <|> AP.endOfInput)
                    return . ToolOutput $ B.unpack line)
                <?> "parseLines"
            parseInitialLines = parseLines (parseInitialPrompt clr)
            parseFollowinglines = parseLines (parseFollowingPrompt clr)
        result <- E.run $ (enumHandle 2048 output $= EB.filter (/= 13))
                    $$ outputSequence (iterParser parseInitialLines) (iterParser parseFollowinglines)
                    $$ sendErrors
        case result of
            Left e  -> putStrLn $ show e
            Right _ -> return ()
        return ()
      where
        sendErrors = E.continue (loop 0 False)
            where
                loop counter errSynced (E.Chunks xs) = do
                    forM_ xs $ \x -> do
                        liftIO $ debugM "leksah-server" $ show x
                        case x of
                            ToolPrompt -> do
                                case (counter, errSynced, errorSyncCommand clr) of
                                    (0, _, _) -> do
                                        E.continue (loop (counter+1) errSynced)
                                    (_, False, Just syncCmd) -> do
                                        liftIO $ hPutStrLn inp $ syncCmd counter
                                        liftIO $ hFlush inp
                                        liftIO $ waitForError counter
                                        E.continue (loop (counter+1) True)
                                    _ -> do
                                        liftIO $ putMVar mvar $ RawToolOutput ToolPrompt
                                        E.continue (loop (counter+1) False)
                            _ -> do
                                liftIO $ putMVar mvar $ RawToolOutput $ x
                                E.continue (loop counter errSynced)
                loop _ _ E.EOF = E.yield () E.EOF

        waitForError counter = do
            foundCount <- takeMVar foundExpectedError
            when (foundCount < counter) $ waitForError counter


fromRawOutput :: RawToolOutput -> [ToolOutput]
fromRawOutput (RawToolOutput output) = [output]
fromRawOutput (_) = []

getOutputNoPrompt :: MonadIO m => Handle -> Handle -> Handle -> ProcessHandle -> IO (Enumerator ToolOutput m b)
getOutputNoPrompt inp out err pid = do
    output <- getOutput noInputCommandLineReader inp out err pid
    return $ output $= EL.concatMap fromRawOutput

newGhci :: [String] -> [String] -> (E.Iteratee ToolOutput IO ()) -> IO ToolState
newGhci buildFlags interactiveFlags startupOutputHandler = do
        tool <- newToolState
        writeChan (toolCommands tool) $
            ToolCommand (":set prompt " ++ B.unpack ghciPrompt) startupOutputHandler
        debugM "leksah-server" $ "Working out GHCi options"
        forkIO $ do
            (out, _) <- runTool "cabal" (["build","--with-ghc=leksahecho"] ++ buildFlags) Nothing
            output <- E.run_ $ out $$ EL.consume
            case catMaybes $ map (findMake . toolline) output of
                options:_ -> do
                        let newOptions = filterUnwanted options
                        debugM "leksah-server" $ newOptions
                        debugM "leksah-server" $ "Starting GHCi"
                        debugM "leksah-server" $ unwords (words newOptions ++ ["-fforce-recomp"] ++ interactiveFlags)
                        runInteractiveTool tool ghciCommandLineReader "ghci"
                            (words newOptions ++ ["-fforce-recomp"] ++ interactiveFlags)
                _ -> do
                    E.run $ E.enumList 1 output $$ startupOutputHandler
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


executeCommand :: ToolState -> String -> E.Iteratee ToolOutput IO () -> IO ()
executeCommand tool command handler = do
    writeChan (toolCommands tool) $ ToolCommand command handler

executeGhciCommand :: ToolState -> String -> E.Iteratee ToolOutput IO () -> IO ()
executeGhciCommand tool command handler = do
    if '\n' `elem` command
        then executeCommand tool safeCommand $ (fixInput =$ (fixOutput =$ handler))
        else executeCommand tool command handler
    where
        filteredLines = (filter safeLine (lines command))
        promptCount = (length filteredLines)+1
        safeCommand = (unlines ([":{"] ++ filteredLines)) ++ ":}"
        safeLine ":{" = False
        safeLine ":}" = False
        safeLine _ = True
        fixOutput = EL.mapAccumM fixOutput' True
        fixOutput' True (ToolOutput line) = return (False, ToolOutput (removePrompts line line promptCount))
        fixOutput' isFirst x = return (isFirst, x)
        fixInput = EL.filter (\x -> (x /= ToolInput ":{") && (x /= ToolInput ":}"))
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

