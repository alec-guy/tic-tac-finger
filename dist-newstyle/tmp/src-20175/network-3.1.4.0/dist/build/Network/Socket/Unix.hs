{-# LINE 1 "Network\\Socket\\Unix.hsc" #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}


#include "HsNetDef.h"

module Network.Socket.Unix (
    isUnixDomainSocketAvailable
  , socketPair
  , sendFd
  , recvFd
  , getPeerCredential
  , getPeerCred
  , getPeerEid
  ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Network.Socket.Buffer
import Network.Socket.Fcntl
import Network.Socket.Imports
import Network.Socket.Types
import System.Posix.Types (Fd(..))


{-# LINE 25 "Network\\Socket\\Unix.hsc" #-}
import Network.Socket.Syscall
import Network.Socket.Win32.Cmsg
import System.Directory
import System.IO
import System.IO.Temp

{-# LINE 35 "Network\\Socket\\Unix.hsc" #-}


{-# LINE 39 "Network\\Socket\\Unix.hsc" #-}

{-# LINE 42 "Network\\Socket\\Unix.hsc" #-}


{-# LINE 46 "Network\\Socket\\Unix.hsc" #-}

-- | Getting process ID, user ID and group ID for UNIX-domain sockets.
--
--   This is implemented with SO_PEERCRED on Linux and getpeereid()
--   on BSD variants. Unfortunately, on some BSD variants
--   getpeereid() returns unexpected results, rather than an error,
--   for AF_INET sockets. It is the user's responsibility to make sure
--   that the socket is a UNIX-domain socket.
--   Also, on some BSD variants, getpeereid() does not return credentials
--   for sockets created via 'socketPair', only separately created and then
--   explicitly connected UNIX-domain sockets work on such systems.
--
--   Since 2.7.0.0.
getPeerCredential :: Socket -> IO (Maybe CUInt, Maybe CUInt, Maybe CUInt)

{-# LINE 75 "Network\\Socket\\Unix.hsc" #-}
getPeerCredential _ = return (Nothing, Nothing, Nothing)

{-# LINE 77 "Network\\Socket\\Unix.hsc" #-}

-- | Returns the processID, userID and groupID of the peer of
--   a UNIX-domain socket.
--
-- Only available on platforms that support SO_PEERCRED.
getPeerCred :: Socket -> IO (CUInt, CUInt, CUInt)

{-# LINE 100 "Network\\Socket\\Unix.hsc" #-}
getPeerCred _ = return (0, 0, 0)

{-# LINE 102 "Network\\Socket\\Unix.hsc" #-}
{-# Deprecated getPeerCred "Use getPeerCredential instead" #-}

-- | Returns the userID and groupID of the peer of
--   a UNIX-domain socket.
--
--  Only available on platforms that support getpeereid().
getPeerEid :: Socket -> IO (CUInt, CUInt)

{-# LINE 123 "Network\\Socket\\Unix.hsc" #-}
getPeerEid _ = return (0, 0)

{-# LINE 125 "Network\\Socket\\Unix.hsc" #-}

{-# Deprecated getPeerEid "Use getPeerCredential instead" #-}

-- | Whether or not UNIX-domain sockets are available.
--   'AF_UNIX' is supported on Windows since 3.1.3.0.
--   So, this variable is 'True` on all platforms.
--
--   Since 2.7.0.0.
isUnixDomainSocketAvailable :: Bool
isUnixDomainSocketAvailable = True

data NullSockAddr = NullSockAddr

instance SocketAddress NullSockAddr where
    sizeOfSocketAddress _ = 0
    peekSocketAddress _   = return NullSockAddr
    pokeSocketAddress _ _ = return ()

-- | Send a file descriptor over a UNIX-domain socket.
--   This function does not work on Windows.
sendFd :: Socket -> CInt -> IO ()
sendFd s outfd = void $ allocaBytes dummyBufSize $ \buf -> do
    let cmsg = encodeCmsg $ Fd outfd
    sendBufMsg s NullSockAddr [(buf,dummyBufSize)] [cmsg] mempty
  where
    dummyBufSize = 1

-- | Receive a file descriptor over a UNIX-domain socket. Note that the resulting
--   file descriptor may have to be put into non-blocking mode in order to be
--   used safely. See 'setNonBlockIfNeeded'.
--   This function does not work on Windows.
recvFd :: Socket -> IO CInt
recvFd s = allocaBytes dummyBufSize $ \buf -> do
    (NullSockAddr, _, cmsgs, _) <- recvBufMsg s [(buf,dummyBufSize)] 32 mempty
    case (lookupCmsg CmsgIdFd cmsgs >>= decodeCmsg) :: Maybe Fd of
      Nothing      -> return (-1)
      Just (Fd fd) -> return fd
  where
    dummyBufSize = 16

-- | Build a pair of connected socket objects.
--   On Windows, this function emulates socketpair() using
--   'AF_UNIX' and a temporary file will remain.
socketPair :: Family              -- Family Name (usually AF_UNIX)
           -> SocketType          -- Socket Type (usually Stream)
           -> ProtocolNumber      -- Protocol Number
           -> IO (Socket, Socket) -- unnamed and connected.

{-# LINE 173 "Network\\Socket\\Unix.hsc" #-}
socketPair _ _ _ = withSystemTempFile "temp-for-pair" $ \file hdl -> do
    hClose hdl
    removeFile file
    listenSock <- socket AF_UNIX Stream defaultProtocol
    bind listenSock $ SockAddrUnix file
    listen listenSock 10
    clientSock <- socket AF_UNIX Stream defaultProtocol
    connect clientSock $ SockAddrUnix file
    (serverSock, _ :: SockAddr) <- accept listenSock
    close listenSock
    withFdSocket clientSock setNonBlockIfNeeded
    withFdSocket serverSock setNonBlockIfNeeded
    return (clientSock, serverSock)

{-# LINE 202 "Network\\Socket\\Unix.hsc" #-}
