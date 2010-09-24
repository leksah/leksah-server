{-# OPTIONS_GHC -XFlexibleInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Utils.Server
-- Copyright   :  2007-2010 Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GPL
--
-- Maintainer  :  maintainer@leksah.org
-- Stability   :  provisional
-- Portability :
--
-- |
--
------------------------------------------------------------------------------

module IDE.Utils.Server
	( ipAddress
	, Server (..)
	, serveOne
	, serveMany
	, ServerRoutine
	, UserAndGroup (..)
	, WaitFor (waitFor))
where

import Network
import Network.Socket hiding (accept)

import System.IO
import Control.Concurrent
import Control.Exception hiding (catch)

import Data.Word
import System.Log.Logger (infoM)

data UserAndGroup = UserAndGroup String String | UserWithDefaultGroup String

-- | Set the user and group for the process. If the group is Nothing, then use the users default group.
-- This is especially useful when you are root and want to become a user.
setUserAndGroup :: UserAndGroup -> IO ()
setUserAndGroup _ = return ()
				
-- | make an IP Address: (127,0,0,1) is the localhost
ipAddress :: (Word8, Word8, Word8, Word8) -> HostAddress
ipAddress (a, b, c, d) = fromIntegral a + 0x100 * fromIntegral b + 0x10000 * fromIntegral c + 0x1000000 * fromIntegral d

-- | the functionality of a server
type ServerRoutine = (Handle, HostName, PortNumber) -> IO ()

serverSocket' :: Server -> IO Socket
serverSocket' (Server (SockAddrInet _ _) t _) = socket AF_INET t defaultProtocol
serverSocket' _ = fail "Unexpected Socket Address Type"

serverSocket :: Server -> IO (Socket, Server)
serverSocket server = do
	sock <- serverSocket' server
	setSocketOption sock ReuseAddr 1
	bindSocket sock (serverAddr server)
	infoM "leksah-server" $ ("Bind " ++ show (serverAddr server))
	listen sock maxListenQueue
	return (sock, server)

-- |the specification of a serving process
data Server = Server {
	serverAddr :: SockAddr,
	serverTyp :: SocketType,
	serverRoutine :: ServerRoutine}

startAccepting :: (Socket, Server) -> IO (ThreadId, MVar ())
startAccepting (sock, server) = do
	mvar <- newEmptyMVar
	threadId <- forkIO (acceptance sock (serverRoutine server) `finally` putMVar mvar ())
	return (threadId, mvar)

serveMany :: Maybe UserAndGroup -> [Server] -> IO [(ThreadId, MVar ())]
serveMany (Just userAndGroup) l = do
	ready <- mapM serverSocket l
	setUserAndGroup userAndGroup
	mapM startAccepting ready
serveMany Nothing l = mapM serverSocket l >>= mapM startAccepting

serveOne :: Maybe UserAndGroup -> Server -> IO (ThreadId, MVar ())
serveOne ug s = do
	l <- serveMany ug [s]
	return (head l)

class WaitFor a where
	waitFor :: a -> IO ()

instance WaitFor (MVar a) where
	waitFor mvar = readMVar mvar >> return ()

instance WaitFor a => WaitFor [a] where
	waitFor = mapM_ waitFor

instance WaitFor (ThreadId, MVar ()) where
	waitFor (_, mvar) = waitFor mvar

acceptance :: Socket -> ServerRoutine -> IO ()
acceptance sock action = catch (do
		dta <- accept sock
		forkIO (action dta) >> return ()) print >>
		acceptance sock action



