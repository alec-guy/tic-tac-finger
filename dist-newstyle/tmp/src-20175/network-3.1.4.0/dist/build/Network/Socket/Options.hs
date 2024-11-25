{-# LINE 1 "Network\\Socket\\Options.hsc" #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}


#include "HsNetDef.h"

module Network.Socket.Options (
    SocketOption(SockOpt
                ,UnsupportedSocketOption
                ,AcceptConn,Debug,ReuseAddr,SoDomain,Type,SoProtocol,SoError
                ,DontRoute,Broadcast,SendBuffer,RecvBuffer,KeepAlive,OOBInline
                ,TimeToLive,MaxSegment,NoDelay,Cork,Linger,ReusePort
                ,RecvLowWater,SendLowWater,RecvTimeOut,SendTimeOut
                ,UseLoopBack,UserTimeout,IPv6Only
                ,RecvIPv4TTL,RecvIPv4TOS,RecvIPv4PktInfo
                ,RecvIPv6HopLimit,RecvIPv6TClass,RecvIPv6PktInfo
                ,CustomSockOpt)
  , isSupportedSocketOption
  , whenSupported
  , getSocketType
  , getSocketOption
  , setSocketOption
  , getSockOpt
  , setSockOpt
  , StructLinger (..)
  , SocketTimeout (..)
  ) where

import qualified Text.Read as P

import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)

import Network.Socket.Imports
import Network.Socket.Internal
import Network.Socket.Types
import Network.Socket.ReadShow



----------------------------------------------------------------
-- Socket Properties

-- | Socket options for use with 'setSocketOption' and 'getSocketOption'.
--
-- The existence of a constructor does not imply that the relevant option
-- is supported on your system: see 'isSupportedSocketOption'
data SocketOption = SockOpt

{-# LINE 53 "Network\\Socket\\Options.hsc" #-}
    !CInt -- ^ Option Level
    !CInt -- ^ Option Name

{-# LINE 59 "Network\\Socket\\Options.hsc" #-}
  deriving (Eq)

----------------------------------------------------------------

socketOptionBijection :: Bijection SocketOption String
socketOptionBijection =
    [ (UnsupportedSocketOption, "UnsupportedSocketOption")
    , (Debug, "Debug")
    , (ReuseAddr, "ReuseAddr")
    , (SoDomain, "SoDomain")
    , (Type, "Type")
    , (SoProtocol, "SoProtocol")
    , (SoError, "SoError")
    , (DontRoute, "DontRoute")
    , (Broadcast, "Broadcast")
    , (SendBuffer, "SendBuffer")
    , (RecvBuffer, "RecvBuffer")
    , (KeepAlive, "KeepAlive")
    , (OOBInline, "OOBInline")
    , (Linger, "Linger")
    , (ReusePort, "ReusePort")
    , (RecvLowWater, "RecvLowWater")
    , (SendLowWater, "SendLowWater")
    , (RecvTimeOut, "RecvTimeOut")
    , (SendTimeOut, "SendTimeOut")
    , (UseLoopBack, "UseLoopBack")
    , (MaxSegment, "MaxSegment")
    , (NoDelay, "NoDelay")
    , (UserTimeout, "UserTimeout")
    , (Cork, "Cork")
    , (TimeToLive, "TimeToLive")
    , (RecvIPv4TTL, "RecvIPv4TTL")
    , (RecvIPv4TOS, "RecvIPv4TOS")
    , (RecvIPv4PktInfo, "RecvIPv4PktInfo")
    , (IPv6Only, "IPv6Only")
    , (RecvIPv6HopLimit, "RecvIPv6HopLimit")
    , (RecvIPv6TClass, "RecvIPv6TClass")
    , (RecvIPv6PktInfo, "RecvIPv6PktInfo")
    ]

instance Show SocketOption where
    showsPrec = bijectiveShow socketOptionBijection def
      where
        defname = "SockOpt"
        unwrap = \(CustomSockOpt nm) -> nm
        def = defShow defname unwrap showIntInt


instance Read SocketOption where
    readPrec = bijectiveRead socketOptionBijection def
      where
        defname = "SockOpt"
        def = defRead defname CustomSockOpt readIntInt

----------------------------------------------------------------

pattern UnsupportedSocketOption :: SocketOption
pattern UnsupportedSocketOption = SockOpt (-1) (-1)

-- | Does the 'SocketOption' exist on this system?
isSupportedSocketOption :: SocketOption -> Bool
isSupportedSocketOption opt = opt /= SockOpt (-1) (-1)

-- | Execute the given action only when the specified socket option is
--  supported. Any return value is ignored.
whenSupported :: SocketOption -> IO a -> IO ()
whenSupported s action
  | isSupportedSocketOption s = action >> return ()
  | otherwise                 = return ()

----------------------------------------------------------------


{-# LINE 132 "Network\\Socket\\Options.hsc" #-}
-- | SO_ACCEPTCONN, read-only
pattern AcceptConn :: SocketOption

{-# LINE 135 "Network\\Socket\\Options.hsc" #-}
pattern AcceptConn     = SockOpt (65535) (2)
{-# LINE 136 "Network\\Socket\\Options.hsc" #-}

{-# LINE 139 "Network\\Socket\\Options.hsc" #-}
-- | SO_DEBUG
pattern Debug :: SocketOption

{-# LINE 142 "Network\\Socket\\Options.hsc" #-}
pattern Debug          = SockOpt (65535) (1)
{-# LINE 143 "Network\\Socket\\Options.hsc" #-}

{-# LINE 146 "Network\\Socket\\Options.hsc" #-}
-- | SO_REUSEADDR
pattern ReuseAddr :: SocketOption

{-# LINE 149 "Network\\Socket\\Options.hsc" #-}
pattern ReuseAddr      = SockOpt (65535) (4)
{-# LINE 150 "Network\\Socket\\Options.hsc" #-}

{-# LINE 153 "Network\\Socket\\Options.hsc" #-}

-- | SO_DOMAIN, read-only
pattern SoDomain :: SocketOption

{-# LINE 159 "Network\\Socket\\Options.hsc" #-}
pattern SoDomain       = SockOpt (-1) (-1)

{-# LINE 161 "Network\\Socket\\Options.hsc" #-}

-- | SO_TYPE, read-only
pattern Type :: SocketOption

{-# LINE 165 "Network\\Socket\\Options.hsc" #-}
pattern Type           = SockOpt (65535) (4104)
{-# LINE 166 "Network\\Socket\\Options.hsc" #-}

{-# LINE 169 "Network\\Socket\\Options.hsc" #-}

-- | SO_PROTOCOL, read-only
pattern SoProtocol :: SocketOption

{-# LINE 175 "Network\\Socket\\Options.hsc" #-}
pattern SoProtocol     = SockOpt (-1) (-1)

{-# LINE 177 "Network\\Socket\\Options.hsc" #-}

-- | SO_ERROR
pattern SoError :: SocketOption

{-# LINE 181 "Network\\Socket\\Options.hsc" #-}
pattern SoError        = SockOpt (65535) (4103)
{-# LINE 182 "Network\\Socket\\Options.hsc" #-}

{-# LINE 185 "Network\\Socket\\Options.hsc" #-}
-- | SO_DONTROUTE
pattern DontRoute :: SocketOption

{-# LINE 188 "Network\\Socket\\Options.hsc" #-}
pattern DontRoute      = SockOpt (65535) (16)
{-# LINE 189 "Network\\Socket\\Options.hsc" #-}

{-# LINE 192 "Network\\Socket\\Options.hsc" #-}
-- | SO_BROADCAST
pattern Broadcast :: SocketOption

{-# LINE 195 "Network\\Socket\\Options.hsc" #-}
pattern Broadcast      = SockOpt (65535) (32)
{-# LINE 196 "Network\\Socket\\Options.hsc" #-}

{-# LINE 199 "Network\\Socket\\Options.hsc" #-}
-- | SO_SNDBUF
pattern SendBuffer :: SocketOption

{-# LINE 202 "Network\\Socket\\Options.hsc" #-}
pattern SendBuffer     = SockOpt (65535) (4097)
{-# LINE 203 "Network\\Socket\\Options.hsc" #-}

{-# LINE 206 "Network\\Socket\\Options.hsc" #-}
-- | SO_RCVBUF
pattern RecvBuffer :: SocketOption

{-# LINE 209 "Network\\Socket\\Options.hsc" #-}
pattern RecvBuffer     = SockOpt (65535) (4098)
{-# LINE 210 "Network\\Socket\\Options.hsc" #-}

{-# LINE 213 "Network\\Socket\\Options.hsc" #-}
-- | SO_KEEPALIVE
pattern KeepAlive :: SocketOption

{-# LINE 216 "Network\\Socket\\Options.hsc" #-}
pattern KeepAlive      = SockOpt (65535) (8)
{-# LINE 217 "Network\\Socket\\Options.hsc" #-}

{-# LINE 220 "Network\\Socket\\Options.hsc" #-}
-- | SO_OOBINLINE
pattern OOBInline :: SocketOption

{-# LINE 223 "Network\\Socket\\Options.hsc" #-}
pattern OOBInline      = SockOpt (65535) (256)
{-# LINE 224 "Network\\Socket\\Options.hsc" #-}

{-# LINE 227 "Network\\Socket\\Options.hsc" #-}
-- | SO_LINGER: timeout in seconds, 0 means disabling/disabled.
pattern Linger :: SocketOption

{-# LINE 230 "Network\\Socket\\Options.hsc" #-}
pattern Linger         = SockOpt (65535) (128)
{-# LINE 231 "Network\\Socket\\Options.hsc" #-}

{-# LINE 234 "Network\\Socket\\Options.hsc" #-}
-- | SO_REUSEPORT
pattern ReusePort :: SocketOption

{-# LINE 239 "Network\\Socket\\Options.hsc" #-}
pattern ReusePort      = SockOpt (-1) (-1)

{-# LINE 241 "Network\\Socket\\Options.hsc" #-}
-- | SO_RCVLOWAT
pattern RecvLowWater :: SocketOption

{-# LINE 244 "Network\\Socket\\Options.hsc" #-}
pattern RecvLowWater   = SockOpt (65535) (4100)
{-# LINE 245 "Network\\Socket\\Options.hsc" #-}

{-# LINE 248 "Network\\Socket\\Options.hsc" #-}
-- | SO_SNDLOWAT
pattern SendLowWater :: SocketOption

{-# LINE 251 "Network\\Socket\\Options.hsc" #-}
pattern SendLowWater   = SockOpt (65535) (4099)
{-# LINE 252 "Network\\Socket\\Options.hsc" #-}

{-# LINE 255 "Network\\Socket\\Options.hsc" #-}
-- | SO_RCVTIMEO: timeout in microseconds
pattern RecvTimeOut :: SocketOption

{-# LINE 258 "Network\\Socket\\Options.hsc" #-}
pattern RecvTimeOut    = SockOpt (65535) (4102)
{-# LINE 259 "Network\\Socket\\Options.hsc" #-}

{-# LINE 262 "Network\\Socket\\Options.hsc" #-}
-- | SO_SNDTIMEO: timeout in microseconds
pattern SendTimeOut :: SocketOption

{-# LINE 265 "Network\\Socket\\Options.hsc" #-}
pattern SendTimeOut    = SockOpt (65535) (4101)
{-# LINE 266 "Network\\Socket\\Options.hsc" #-}

{-# LINE 269 "Network\\Socket\\Options.hsc" #-}
-- | SO_USELOOPBACK
pattern UseLoopBack :: SocketOption

{-# LINE 272 "Network\\Socket\\Options.hsc" #-}
pattern UseLoopBack    = SockOpt (65535) (64)
{-# LINE 273 "Network\\Socket\\Options.hsc" #-}

{-# LINE 276 "Network\\Socket\\Options.hsc" #-}

{-# LINE 277 "Network\\Socket\\Options.hsc" #-}


{-# LINE 308 "Network\\Socket\\Options.hsc" #-}


{-# LINE 341 "Network\\Socket\\Options.hsc" #-}


{-# LINE 374 "Network\\Socket\\Options.hsc" #-}

pattern CustomSockOpt :: (CInt, CInt) -> SocketOption
pattern CustomSockOpt xy <- ((\(SockOpt x y) -> (x, y)) -> xy)
  where
    CustomSockOpt (x, y) = SockOpt x y

----------------------------------------------------------------

-- | Set a socket option that expects an 'Int' value.
setSocketOption :: Socket
                -> SocketOption -- Option Name
                -> Int          -- Option Value
                -> IO ()

{-# LINE 388 "Network\\Socket\\Options.hsc" #-}
setSocketOption s so@Linger v = do
    let arg = if v == 0 then StructLinger 0 0 else StructLinger 1 (fromIntegral v)
    setSockOpt s so arg

{-# LINE 392 "Network\\Socket\\Options.hsc" #-}
setSocketOption s so@RecvTimeOut v = setSockOpt s so $ SocketTimeout $ fromIntegral v
setSocketOption s so@SendTimeOut v = setSockOpt s so $ SocketTimeout $ fromIntegral v
setSocketOption s sa v = setSockOpt s sa (fromIntegral v :: CInt)

-- | Set a socket option.
setSockOpt :: Storable a
           => Socket
           -> SocketOption
           -> a
           -> IO ()
setSockOpt s (SockOpt level opt) v = do
    with v $ \ptr -> void $ do
        let sz = fromIntegral $ sizeOf v
        withFdSocket s $ \fd ->
          throwSocketErrorIfMinus1_ "Network.Socket.setSockOpt" $
          c_setsockopt fd level opt ptr sz

----------------------------------------------------------------

-- | Get a socket option that gives an 'Int' value.
getSocketOption :: Socket
                -> SocketOption  -- Option Name
                -> IO Int        -- Option Value

{-# LINE 416 "Network\\Socket\\Options.hsc" #-}
getSocketOption s so@Linger = do
    StructLinger onoff linger <- getSockOpt s so
    return $ fromIntegral $ if onoff == 0 then 0 else linger

{-# LINE 420 "Network\\Socket\\Options.hsc" #-}
getSocketOption s so@RecvTimeOut = do
    SocketTimeout to <- getSockOpt s so
    return $ fromIntegral to
getSocketOption s so@SendTimeOut = do
    SocketTimeout to <- getSockOpt s so
    return $ fromIntegral to
getSocketOption s so = do
    n :: CInt <- getSockOpt s so
    return $ fromIntegral n

-- | Get a socket option.
getSockOpt :: forall a . Storable a
           => Socket
           -> SocketOption -- Option Name
           -> IO a         -- Option Value
getSockOpt s (SockOpt level opt) = do
    alloca $ \ptr -> do
        let sz = fromIntegral $ sizeOf (undefined :: a)
        withFdSocket s $ \fd -> with sz $ \ptr_sz -> do
            throwSocketErrorIfMinus1Retry_ "Network.Socket.getSockOpt" $
                c_getsockopt fd level opt ptr ptr_sz
        peek ptr

----------------------------------------------------------------

-- | Get the 'SocketType' of an active socket.
--
--   Since: 3.0.1.0
getSocketType :: Socket -> IO SocketType
getSocketType s = unpackSocketType <$> getSockOpt s Type

----------------------------------------------------------------


{-# LINE 454 "Network\\Socket\\Options.hsc" #-}
{-# COMPLETE CustomSockOpt #-}

{-# LINE 456 "Network\\Socket\\Options.hsc" #-}

{-# LINE 457 "Network\\Socket\\Options.hsc" #-}
-- | Low level 'SO_LINBER' option value, which can be used with 'setSockOpt'.
--
data StructLinger = StructLinger {
    -- | Set the linger option on.
    sl_onoff  :: CInt,

    -- | Linger timeout.
    sl_linger :: CInt
  }
  deriving (Eq, Ord, Show)

instance Storable StructLinger where
    sizeOf    _ = (4)
{-# LINE 470 "Network\\Socket\\Options.hsc" #-}
    alignment _ = alignment (0 :: CInt)

    peek p = do
        onoff  <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
{-# LINE 474 "Network\\Socket\\Options.hsc" #-}
        linger <- ((\hsc_ptr -> peekByteOff hsc_ptr 2)) p
{-# LINE 475 "Network\\Socket\\Options.hsc" #-}
        return $ StructLinger onoff linger

    poke p (StructLinger onoff linger) = do
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0))  p onoff
{-# LINE 479 "Network\\Socket\\Options.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 2)) p linger
{-# LINE 480 "Network\\Socket\\Options.hsc" #-}

{-# LINE 481 "Network\\Socket\\Options.hsc" #-}

----------------------------------------------------------------

-- | Timeout in microseconds.
--   This will be converted into struct timeval on Unix and
--   DWORD (as milliseconds) on Windows.
newtype SocketTimeout = SocketTimeout Word32 deriving (Eq, Ord, Show)


{-# LINE 490 "Network\\Socket\\Options.hsc" #-}
instance Storable SocketTimeout where
    sizeOf (SocketTimeout to) = sizeOf to -- DWORD as milliseconds
    alignment _ = 0
    peek ptr    = do
        to <- peek (castPtr ptr)
        return $ SocketTimeout (to * 1000)
    poke ptr (SocketTimeout to) = poke (castPtr ptr) (to `div` 1000)

{-# LINE 510 "Network\\Socket\\Options.hsc" #-}

----------------------------------------------------------------

foreign import CALLCONV unsafe "getsockopt"
  c_getsockopt :: CInt -> CInt -> CInt -> Ptr a -> Ptr CInt -> IO CInt
foreign import CALLCONV unsafe "setsockopt"
  c_setsockopt :: CInt -> CInt -> CInt -> Ptr a -> CInt -> IO CInt
