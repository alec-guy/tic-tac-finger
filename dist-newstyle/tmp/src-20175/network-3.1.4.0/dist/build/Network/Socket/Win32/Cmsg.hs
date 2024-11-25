{-# LINE 1 "Network\\Socket\\Win32\\Cmsg.hsc" #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Network.Socket.Win32.Cmsg where



import Data.ByteString.Internal
import System.Posix.Types (Fd(..))
import Foreign.ForeignPtr
import System.IO.Unsafe (unsafeDupablePerformIO)

import Network.Socket.Imports
import Network.Socket.Types
import Network.Socket.ReadShow

import qualified Text.Read as P

type DWORD = Word32
type ULONG = Word32

-- | Control message (ancillary data) including a pair of level and type.
data Cmsg = Cmsg {
    cmsgId   :: !CmsgId
  , cmsgData :: !ByteString
  } deriving (Eq, Show)

----------------------------------------------------------------

-- | Identifier of control message (ancillary data).
data CmsgId = CmsgId {
    cmsgLevel :: !CInt
  , cmsgType  :: !CInt
  } deriving (Eq)

-- | Unsupported identifier
pattern UnsupportedCmsgId :: CmsgId
pattern UnsupportedCmsgId = CmsgId (-1) (-1)

-- | The identifier for 'IPv4TTL'.
pattern CmsgIdIPv4TTL :: CmsgId
pattern CmsgIdIPv4TTL = CmsgId (0) (4)
{-# LINE 49 "Network\\Socket\\Win32\\Cmsg.hsc" #-}

-- | The identifier for 'IPv6HopLimit'.
pattern CmsgIdIPv6HopLimit :: CmsgId
pattern CmsgIdIPv6HopLimit = CmsgId (41) (21)
{-# LINE 53 "Network\\Socket\\Win32\\Cmsg.hsc" #-}

-- | The identifier for 'IPv4TOS'.
pattern CmsgIdIPv4TOS :: CmsgId
pattern CmsgIdIPv4TOS = CmsgId (0) (3)
{-# LINE 57 "Network\\Socket\\Win32\\Cmsg.hsc" #-}

-- | The identifier for 'IPv6TClass'.
pattern CmsgIdIPv6TClass :: CmsgId
pattern CmsgIdIPv6TClass = CmsgId (41) (39)
{-# LINE 61 "Network\\Socket\\Win32\\Cmsg.hsc" #-}

-- | The identifier for 'IPv4PktInfo'.
pattern CmsgIdIPv4PktInfo :: CmsgId
pattern CmsgIdIPv4PktInfo = CmsgId (0) (19)
{-# LINE 65 "Network\\Socket\\Win32\\Cmsg.hsc" #-}

-- | The identifier for 'IPv6PktInfo'.
pattern CmsgIdIPv6PktInfo :: CmsgId
pattern CmsgIdIPv6PktInfo = CmsgId (41) (19)
{-# LINE 69 "Network\\Socket\\Win32\\Cmsg.hsc" #-}

-- | Control message ID for POSIX file-descriptor passing.
--
--  Not supported on Windows; use WSADuplicateSocket instead
pattern CmsgIdFd :: CmsgId
pattern CmsgIdFd = CmsgId (-1) (-1)

----------------------------------------------------------------

-- | Looking up control message. The following shows an example usage:
--
-- > (lookupCmsg CmsgIdIPv4TOS cmsgs >>= decodeCmsg) :: Maybe IPv4TOS
lookupCmsg :: CmsgId -> [Cmsg] -> Maybe Cmsg
lookupCmsg _   [] = Nothing
lookupCmsg cid (cmsg:cmsgs)
  | cmsgId cmsg == cid = Just cmsg
  | otherwise          = lookupCmsg cid cmsgs

-- | Filtering control message.
filterCmsg :: CmsgId -> [Cmsg] -> [Cmsg]
filterCmsg cid cmsgs = filter (\cmsg -> cmsgId cmsg == cid) cmsgs

----------------------------------------------------------------

-- | A class to encode and decode control message.
class Storable a => ControlMessage a where
    controlMessageId :: CmsgId

encodeCmsg :: forall a. ControlMessage a => a -> Cmsg
encodeCmsg x = unsafeDupablePerformIO $ do
    bs <- create siz $ \p0 -> do
        let p = castPtr p0
        poke p x
    let cmsid = controlMessageId @a
    return $ Cmsg cmsid bs
  where
    siz = sizeOf x

decodeCmsg :: forall a . (ControlMessage a, Storable a) => Cmsg -> Maybe a
decodeCmsg (Cmsg cmsid (PS fptr off len))
  | cid /= cmsid = Nothing
  | len < siz    = Nothing
  | otherwise = unsafeDupablePerformIO $ withForeignPtr fptr $ \p0 -> do
        let p = castPtr (p0 `plusPtr` off)
        Just <$> peek p
  where
    cid = controlMessageId @a
    siz = sizeOf (undefined :: a)

----------------------------------------------------------------

-- | Time to live of IPv4.
newtype IPv4TTL = IPv4TTL DWORD deriving (Eq, Show, Storable)

instance ControlMessage IPv4TTL where
    controlMessageId = CmsgIdIPv4TTL

----------------------------------------------------------------

-- | Hop limit of IPv6.
newtype IPv6HopLimit = IPv6HopLimit DWORD deriving (Eq, Show, Storable)

instance ControlMessage IPv6HopLimit where
    controlMessageId = CmsgIdIPv6HopLimit

----------------------------------------------------------------

-- | TOS of IPv4.
newtype IPv4TOS = IPv4TOS DWORD deriving (Eq, Show, Storable)

instance ControlMessage IPv4TOS where
    controlMessageId = CmsgIdIPv4TOS

----------------------------------------------------------------

-- | Traffic class of IPv6.
newtype IPv6TClass = IPv6TClass DWORD deriving (Eq, Show, Storable)

instance ControlMessage IPv6TClass where
    controlMessageId = CmsgIdIPv6TClass

----------------------------------------------------------------

-- | Network interface ID and local IPv4 address. The second member is
--   redundant to be the same as Unix's one and is always 0.0.0.0.
data IPv4PktInfo = IPv4PktInfo Int HostAddress HostAddress deriving (Eq)

instance Show IPv4PktInfo where
    show (IPv4PktInfo n sa ha) = "IPv4PktInfo " ++ show n ++ " " ++ show (hostAddressToTuple sa) ++ " " ++ show (hostAddressToTuple ha)

instance ControlMessage IPv4PktInfo where
    controlMessageId = CmsgIdIPv4PktInfo

instance Storable IPv4PktInfo where
    sizeOf    _ = (8)
{-# LINE 164 "Network\\Socket\\Win32\\Cmsg.hsc" #-}
    alignment _ = 4
{-# LINE 165 "Network\\Socket\\Win32\\Cmsg.hsc" #-}
    poke p (IPv4PktInfo n _ ha) = do
        ((\hsc_ptr -> pokeByteOff hsc_ptr 4))  p (fromIntegral n :: CInt)
{-# LINE 167 "Network\\Socket\\Win32\\Cmsg.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0))     p ha
{-# LINE 168 "Network\\Socket\\Win32\\Cmsg.hsc" #-}
    peek p = do
        n  <- ((\hsc_ptr -> peekByteOff hsc_ptr 4))  p
{-# LINE 170 "Network\\Socket\\Win32\\Cmsg.hsc" #-}
        ha <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))     p
{-# LINE 171 "Network\\Socket\\Win32\\Cmsg.hsc" #-}
        return $ IPv4PktInfo n 0 ha

----------------------------------------------------------------

-- | Network interface ID and local IPv4 address.
data IPv6PktInfo = IPv6PktInfo Int HostAddress6 deriving (Eq)

instance Show IPv6PktInfo where
    show (IPv6PktInfo n ha6) = "IPv6PktInfo " ++ show n ++ " " ++ show (hostAddress6ToTuple ha6)

instance ControlMessage IPv6PktInfo where
    controlMessageId = CmsgIdIPv6PktInfo

instance Storable IPv6PktInfo where
    sizeOf    _ = (20)
{-# LINE 186 "Network\\Socket\\Win32\\Cmsg.hsc" #-}
    alignment _ = 4
{-# LINE 187 "Network\\Socket\\Win32\\Cmsg.hsc" #-}
    poke p (IPv6PktInfo n ha6) = do
        ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) p (fromIntegral n :: CInt)
{-# LINE 189 "Network\\Socket\\Win32\\Cmsg.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0))    p (In6Addr ha6)
{-# LINE 190 "Network\\Socket\\Win32\\Cmsg.hsc" #-}
    peek p = do
        In6Addr ha6 <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))    p
{-# LINE 192 "Network\\Socket\\Win32\\Cmsg.hsc" #-}
        n :: ULONG  <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) p
{-# LINE 193 "Network\\Socket\\Win32\\Cmsg.hsc" #-}
        return $ IPv6PktInfo (fromIntegral n) ha6

instance ControlMessage Fd where
    controlMessageId = CmsgIdFd

cmsgIdBijection :: Bijection CmsgId String
cmsgIdBijection =
    [ (UnsupportedCmsgId, "UnsupportedCmsgId")
    , (CmsgIdIPv4TTL, "CmsgIdIPv4TTL")
    , (CmsgIdIPv6HopLimit, "CmsgIdIPv6HopLimit")
    , (CmsgIdIPv4TOS, "CmsgIdIPv4TOS")
    , (CmsgIdIPv6TClass, "CmsgIdIPv6TClass")
    , (CmsgIdIPv4PktInfo, "CmsgIdIPv4PktInfo")
    , (CmsgIdIPv6PktInfo, "CmsgIdIPv6PktInfo")
    , (CmsgIdFd, "CmsgIdFd")
    ]

instance Show CmsgId where
    showsPrec = bijectiveShow cmsgIdBijection def
      where
        defname = "CmsgId"
        unId = \(CmsgId l t) -> (l,t)
        def = defShow defname unId showIntInt

instance Read CmsgId where
    readPrec = bijectiveRead cmsgIdBijection def
      where
        defname = "CmsgId"
        def = defRead defname (uncurry CmsgId) readIntInt
