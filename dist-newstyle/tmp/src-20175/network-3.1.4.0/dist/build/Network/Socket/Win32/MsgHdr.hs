{-# LINE 1 "Network\\Socket\\Win32\\MsgHdr.hsc" #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE CPP #-}

-- | Support module for the Windows 'WSASendMsg' system call.
module Network.Socket.Win32.MsgHdr
    ( MsgHdr(..)
    ) where



import Network.Socket.Imports
import Network.Socket.Internal (zeroMemory)
import Network.Socket.Win32.WSABuf

type DWORD = Word32
type ULONG = Word32

-- The size of BufferLen is different on pre-vista compilers.
-- But since those platforms are out of support anyway we ignore that.
data MsgHdr sa = MsgHdr
    { msgName      :: !(Ptr sa)
    , msgNameLen   :: !CInt
    , msgBuffer    :: !(Ptr WSABuf)
    , msgBufferLen :: !DWORD
    , msgCtrl      :: !(Ptr Word8)
    , msgCtrlLen   :: !ULONG
    , msgFlags     :: !DWORD
    } deriving Show

instance Storable (MsgHdr sa) where
  sizeOf    _ = (56)
{-# LINE 32 "Network\\Socket\\Win32\\MsgHdr.hsc" #-}
  alignment _ = 8
{-# LINE 33 "Network\\Socket\\Win32\\MsgHdr.hsc" #-}

  peek p = do
    name       <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))          p
{-# LINE 36 "Network\\Socket\\Win32\\MsgHdr.hsc" #-}
    nameLen    <- ((\hsc_ptr -> peekByteOff hsc_ptr 8))       p
{-# LINE 37 "Network\\Socket\\Win32\\MsgHdr.hsc" #-}
    buffer     <- ((\hsc_ptr -> peekByteOff hsc_ptr 16))     p
{-# LINE 38 "Network\\Socket\\Win32\\MsgHdr.hsc" #-}
    bufferLen  <- ((\hsc_ptr -> peekByteOff hsc_ptr 24)) p
{-# LINE 39 "Network\\Socket\\Win32\\MsgHdr.hsc" #-}
    ctrl       <- ((\hsc_ptr -> peekByteOff hsc_ptr 40))   p
{-# LINE 40 "Network\\Socket\\Win32\\MsgHdr.hsc" #-}
    ctrlLen    <- ((\hsc_ptr -> peekByteOff hsc_ptr 32))   p
{-# LINE 41 "Network\\Socket\\Win32\\MsgHdr.hsc" #-}
    flags      <- ((\hsc_ptr -> peekByteOff hsc_ptr 48))       p
{-# LINE 42 "Network\\Socket\\Win32\\MsgHdr.hsc" #-}
    return $ MsgHdr name nameLen buffer bufferLen ctrl ctrlLen flags

  poke p mh = do
    -- We need to zero the msg_control, msg_controllen, and msg_flags
    -- fields, but they only exist on some platforms (e.g. not on
    -- Solaris).  Instead of using CPP, we zero the entire struct.
    zeroMemory p (56)
{-# LINE 49 "Network\\Socket\\Win32\\MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0))           p (msgName       mh)
{-# LINE 50 "Network\\Socket\\Win32\\MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8))        p (msgNameLen    mh)
{-# LINE 51 "Network\\Socket\\Win32\\MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 16))      p (msgBuffer     mh)
{-# LINE 52 "Network\\Socket\\Win32\\MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 24))  p (msgBufferLen  mh)
{-# LINE 53 "Network\\Socket\\Win32\\MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 40))    p (msgCtrl       mh)
{-# LINE 54 "Network\\Socket\\Win32\\MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 32))    p (msgCtrlLen    mh)
{-# LINE 55 "Network\\Socket\\Win32\\MsgHdr.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 48))        p (msgFlags      mh)
{-# LINE 56 "Network\\Socket\\Win32\\MsgHdr.hsc" #-}
