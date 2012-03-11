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
    runProcess

--    waitForChildren,
--    forkChild

) where

import Control.Concurrent
       (tryTakeMVar, readMVar, takeMVar, putMVar,
        newEmptyMVar, forkIO, newChan, MVar, Chan, writeChan,
        getChanContents, dupChan)
import Control.Monad (forM_, when, unless)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.List (stripPrefix)
import Data.Maybe (catMaybes)
#ifdef MIN_VERSION_process_leksah
import IDE.System.Process
       (proc, waitForProcess, ProcessHandle, createProcess, CreateProcess(..),
       interruptProcessGroup, runCommand, getProcessExitCode,
       runProcess, runInteractiveProcess)
import IDE.System.Process.Internals (StdStream(..))
#else
import System.Process
       (proc, waitForProcess, ProcessHandle, createProcess, CreateProcess(..),
       interruptProcessGroupOf, runCommand, getProcessExitCode,
       runProcess, runInteractiveProcess)
import System.Process.Internals (StdStream(..))
#endif
#if MIN_VERSION_base(4,3,0)
import System.IO (hGetBufSome)
import qualified Data.ByteString.Internal as B
       (createAndTrim)
#else
import System.IO (hWaitForInput, hIsEOF)
import qualified Data.ByteString as B
       (hGetNonBlocking)
#endif
import Control.DeepSeq
import System.Log.Logger (debugM)
import System.Exit (ExitCode(..))
import System.IO (hFlush, hPutStrLn, Handle, hSetBuffering, BufferMode(..))
import Control.Applicative ((<|>), Alternative, liftA2, liftA)
--import Data.Enumerator.Binary as E (enumHandle)
import Data.Enumerator as E
       (continue, tryIO, checkContinue0, (=$), (>>==), Stream(..),
        Enumeratee, Enumerator, run, ($$), ($=), (>==>))
import qualified Data.Enumerator as E
       (enumList, returnI, Step(..), isEOF, checkDone, yield,
        Iteratee(..), sequence, run_)
import qualified Data.Enumerator.Binary as EB (filter)
import Data.Attoparsec.Enumerator (iterParser)
import qualified Data.Attoparsec.Char8 as AP
       (endOfInput, takeWhile, satisfy, skipWhile, string, Parser,
        endOfLine, digit, manyTill, takeWhile1)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B (unpack, pack)
import Data.Attoparsec ((<?>))
import qualified Data.Enumerator.List as EL
       (consume, concatMap, concatMapAccumM)
import Data.Char (isDigit)
import qualified System.IO as IO (Handle)
import qualified Data.ByteString as B
       (empty, null, ByteString)
import System.IO.Error (mkIOError, illegalOperationErrorType)

data ToolOutput = ToolInput String
                | ToolError String
                | ToolOutput String
                | ToolPrompt String
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

data ToolCommand = ToolCommand String String (E.Iteratee ToolOutput IO ())
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
toolline (ToolPrompt l)  = l
toolline (ToolExit _code) = ""

quoteArg :: String -> String
quoteArg s | ' ' `elem` s = "\"" ++ (escapeQuotes s) ++ "\""
quoteArg s                = s

escapeQuotes :: String -> String
escapeQuotes = foldr (\c s -> if c == '"' then '\\':c:s else c:s) ""

#ifdef MIN_VERSION_process_leksah
interruptProcessGroupOf :: ProcessHandle -> IO ()
interruptProcessGroupOf = interruptProcessGroup
#endif

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
    isEndOfCommandOutput (ToolPrompt _) = True
    isEndOfCommandOutput (ToolExit _) = True
    isEndOfCommandOutput _ = False

    isolateCommandOutput = isolateToFirst (not . isEndOfCommandOutput)

    processCommand [] _ = do
        liftIO $ debugM "leksah-server" $ "No More Commands"
        return ()
    processCommand ((command@(ToolCommand commandString rawCommandString handler)):remainingCommands) inp = do
        liftIO $ putMVar (currentToolCommand tool) command
        liftIO $ hPutStrLn inp commandString
        liftIO $ hFlush inp

        (E.enumList 1 (map ToolInput (lines rawCommandString)) >==> isolateCommandOutput) =$ handler
        processCommand remainingCommands inp

    outputSequence :: Handle -> E.Enumeratee RawToolOutput ToolOutput IO b
    outputSequence inp =
        EL.concatMapAccumM writeCommandOutput (False, False, (outputSyncCommand clr), 0, "")
      where
        writeCommandOutput (False, False, (Just outSyncCmd), n, _) (RawToolOutput (ToolPrompt line)) = do
            debugM "leksah-server" $ "Pre Sync Prompt"
            hPutStrLn inp $ outSyncCmd n
            hFlush inp
            return ((True, False, (Just outSyncCmd), n, line), [])
        writeCommandOutput (True, False, mbSyncCmd, n, promptLine) (RawToolOutput (ToolPrompt _)) = do
            debugM "leksah-server" $ "Unsynced Prompt"
            return ((True, False, mbSyncCmd, n, promptLine), [])
        writeCommandOutput (True, False, mbSyncCmd, n, promptLine) (RawToolOutput o@(ToolOutput line)) = do
            let synced = (isExpectedOutput clr n line)
            when synced $ debugM "leksah-server" $ "Output Sync Found"
            return ((True, synced, mbSyncCmd, n, promptLine), if synced then [] else [o])
        writeCommandOutput (_, _, mbSyncCmd, n, promptLine) (RawToolOutput (ToolPrompt _)) = do
            debugM "leksah-server" $ "Synced Prompt - Ready For Next Command"
            tryTakeMVar (currentToolCommand tool)
            return ((False, False, mbSyncCmd, n+1, promptLine), [ToolPrompt promptLine])
        writeCommandOutput s (RawToolOutput o@(ToolExit _)) = do
            debugM "leksah-server" $ "Tool Exit"
            putMVar (outputClosed tool) True
            return (s, [o])
        writeCommandOutput s (RawToolOutput o) = do
            return (s, [o])
        writeCommandOutput s x = do
            debugM "leksah-server" $ "Unexpected output " ++ show x
            return (s, [])

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
    parseInitialPrompt :: AP.Parser String,
    parseFollowingPrompt :: AP.Parser String,
    errorSyncCommand :: Maybe (Int -> String),
    parseExpectedError :: AP.Parser (String, Int),
    outputSyncCommand :: Maybe (Int -> String),
    isExpectedOutput :: Int -> String -> Bool
    }

ghciParseInitialPrompt :: AP.Parser String
ghciParseInitialPrompt = (do
        ((AP.string $ B.pack "Prelude") <|> (AP.string $ B.pack "*"))
        AP.skipWhile (\c -> c /= '>' && c/= '\n')
        AP.string $ B.pack "> "
        return "")
    <?> "ghciParseInitialPrompt"

ghciParseFollowingPrompt :: AP.Parser String
ghciParseFollowingPrompt = (do
        AP.satisfy (/='\n') `AP.manyTill` (AP.string $ B.pack $ ghciPrompt))
    <?> "ghciParseFollowingPrompt"

marker :: Int -> String
marker n = "kMAKWRALZZbHdXfHUOAAYBB" ++ show n

parseMarker :: AP.Parser Int
parseMarker = (do
        AP.string $ B.pack "kMAKWRALZZbHdXfHUOAAYBB"
        nums <- AP.takeWhile isDigit
        return . read $ B.unpack nums)
    <?> "parseMarker"

ghciParseExpectedErrorCols :: AP.Parser ()
ghciParseExpectedErrorCols = (do
        AP.string $ B.pack "0-"
        AP.digit
        AP.digit
        return ())
    <|> (do
        AP.string $ B.pack "1-"
        AP.digit
        AP.digit
        return ())
    <|> (do
        AP.string $ B.pack "0"
        return ())
    <|> (do
        AP.string $ B.pack "1"
        return ())
    <?> "ghciParseExpectedErrorCols"

manyTill' :: Alternative f => f a -> f b -> f ([a], b)
manyTill' p end = scan
    where scan = liftA (\b -> ([], b)) end <|> liftA2 (\a (as, b) -> (a:as, b)) p scan

ghciParseExpectedError :: AP.Parser (String, Int)
ghciParseExpectedError = (do
       AP.satisfy (/='\n') `manyTill'` (do
        AP.string $ B.pack "\n<interactive>:"
        AP.takeWhile1 isDigit
        AP.string $ B.pack ":"
        ghciParseExpectedErrorCols
        AP.string $ B.pack ": Not in scope: `"
        result <- parseMarker
        AP.string $ B.pack "'\n"
        return result))
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
    outputSyncCommand    = Just $ \n -> ":set prompt \"" ++ marker n ++ "\\n\"\n:set prompt " ++ ghciPrompt,
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

parseError :: AP.Parser (String, Int) -> AP.Parser (Either (String, Int) ByteString)
parseError expectedErrorParser = (do
        expected <- expectedErrorParser
        return $ Left expected)
    <|> (do
        line <- AP.takeWhile (/= '\n')
        (AP.endOfInput <|> AP.endOfLine)
        return $ Right line)
    <?> "parseError"

-- From enumerator but using hGetSome (to fix Win32)
enumHandle :: MonadIO m
           => Integer -- ^ Buffer size
           -> IO.Handle
           -> Enumerator B.ByteString m b
enumHandle bufferSize h = checkContinue0 $ \loop k -> do
    let intSize = fromInteger bufferSize

    bytes <- tryIO (hGetSome h intSize)
    if B.null bytes
        then continue k
        else k (Chunks [bytes]) >>== loop

-- From byteString (for GHC 6.12.3 support)
hGetSome :: Handle -> Int -> IO ByteString
hGetSome hh i
#if MIN_VERSION_base(4,3,0)
    | i >  0    = B.createAndTrim i $ \p -> hGetBufSome hh p i
#else
    | i >  0    = let
                   loop = do
                     s <- B.hGetNonBlocking hh i
                     if not (B.null s)
                        then return s
                        else do eof <- hIsEOF hh
                                if eof then return s
                                       else hWaitForInput hh (-1) >> loop
                                         -- for this to work correctly, the
                                         -- Handle should be in binary mode
                                         -- (see GHC ticket #3808)
                  in loop
#endif
    | i == 0    = return B.empty
    | otherwise = illegalBufferSize hh "hGetSome" i

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize handle fn sz =
    ioError (mkIOError illegalOperationErrorType msg (Just handle) Nothing)
    --TODO: System.IO uses InvalidArgument here, but it's not exported :-(
    where
      msg = fn ++ ": illegal ByteString size " ++ showsPrec 9 sz []

getOutput :: MonadIO m => CommandLineReader -> Handle -> Handle -> Handle -> ProcessHandle
    -> IO (Enumerator RawToolOutput m b)
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
                            Left (line, counter) -> do
                                unless (null line) $ putMVar mvar $ RawToolOutput $ ToolError line
                                putMVar foundExpectedError counter
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
                    ToolPrompt _ -> k (Chunks [v]) >>== loop i2
                    _ -> k (Chunks [v]) >>== loop i

    readOutput :: MVar RawToolOutput -> Handle -> MVar Int -> IO ()
    readOutput mvar output foundExpectedError = do
        let parseLines parsePrompt = (do
                    lineSoFar <- parsePrompt
                    return $ ToolPrompt lineSoFar)
                <|> (do
                    line <- AP.takeWhile (/= '\n')
                    (AP.endOfInput <|> AP.endOfLine)
                    return . ToolOutput $ B.unpack line)
                <?> "parseLines"
            parseInitialLines = parseLines (parseInitialPrompt clr)
            parseFollowinglines = parseLines (parseFollowingPrompt clr)
        E.run_ $ (enumHandle 2048 output $= EB.filter (/= 13))
                    $$ outputSequence (iterParser parseInitialLines) (iterParser parseFollowinglines)
                    $$ sendErrors
        return ()
      where
        sendErrors = E.continue (loop 0 False "")
            where
                loop counter errSynced promptLine (E.Chunks xs) = do
                    forM_ xs $ \x -> do
                        liftIO $ debugM "leksah-server" $ show x
                        case x of
                            ToolPrompt line -> do
                                case (counter, errSynced, errorSyncCommand clr) of
                                    (0, _, _) -> do
                                        E.continue (loop (counter+1) errSynced line)
                                    (_, False, Just syncCmd) -> do
                                        liftIO $ do
                                            debugM "leksah-server" $ "sendErrors - Sync " ++ show counter
                                            hPutStrLn inp $ syncCmd counter
                                            hFlush inp
                                            waitForError counter
                                            debugM "leksah-server" $ "sendErrors - Synced " ++ show counter
                                        E.continue (loop (counter+1) True line)
                                    (_, True, Just _) -> do
                                        liftIO $ putMVar mvar $ RawToolOutput (ToolPrompt promptLine)
                                        E.continue (loop (counter+1) False promptLine)
                                    _ -> do
                                        liftIO $ putMVar mvar $ RawToolOutput x
                                        E.continue (loop (counter+1) False promptLine)
                            _ -> do
                                liftIO . putMVar mvar $ RawToolOutput x
                                E.continue (loop counter errSynced promptLine)
                loop _ _ _ E.EOF = E.yield () E.EOF

        waitForError counter = do
            foundCount <- takeMVar foundExpectedError
            debugM "leksah-server" $ "waitForError - Found " ++ show foundCount
            when (foundCount < counter) $ waitForError counter


fromRawOutput :: RawToolOutput -> [ToolOutput]
fromRawOutput (RawToolOutput output) = [output]
fromRawOutput (_) = []

getOutputNoPrompt :: MonadIO m => Handle -> Handle -> Handle -> ProcessHandle -> IO (Enumerator ToolOutput m b)
getOutputNoPrompt inp out err pid = do
    output <- getOutput noInputCommandLineReader inp out err pid
    return $ output $= EL.concatMap fromRawOutput

newGhci' :: [String] -> (E.Iteratee ToolOutput IO ()) -> IO ToolState
newGhci' flags startupOutputHandler = do
    tool <- newToolState
    writeChan (toolCommands tool) $
        ToolCommand (":set prompt " ++ ghciPrompt) "" startupOutputHandler
    runInteractiveTool tool ghciCommandLineReader "ghci" flags
    return tool

newGhci :: [String] -> [String] -> (E.Iteratee ToolOutput IO ()) -> IO ToolState
newGhci buildFlags interactiveFlags startupOutputHandler = do
        tool <- newToolState
        writeChan (toolCommands tool) $
            ToolCommand (":set prompt " ++ ghciPrompt) "" startupOutputHandler
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


executeCommand :: ToolState -> String -> String -> E.Iteratee ToolOutput IO () -> IO ()
executeCommand tool command rawCommand handler = do
    writeChan (toolCommands tool) $ ToolCommand command rawCommand handler

executeGhciCommand :: ToolState -> String -> E.Iteratee ToolOutput IO () -> IO ()
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

