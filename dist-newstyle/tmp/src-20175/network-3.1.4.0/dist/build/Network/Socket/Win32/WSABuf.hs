{-# LINE 1 "Network\\Socket\\Win32\\WSABuf.hsc" #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Support module for the Windows winsock system calls.
module Network.Socket.Win32.WSABuf
    ( WSABuf(..)
    , withWSABuf
    ) where



import Foreign.Marshal.Array (allocaArray)

import Network.Socket.Imports

type ULONG = Word32

data WSABuf = WSABuf
    { wsaBufPtr :: !(Ptr Word8)
    , wsaBufLen :: !ULONG
    }

instance Storable WSABuf where
  sizeOf    _ = (16)
{-# LINE 24 "Network\\Socket\\Win32\\WSABuf.hsc" #-}
  alignment _ = 8
{-# LINE 25 "Network\\Socket\\Win32\\WSABuf.hsc" #-}

  peek p = do
    base <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 28 "Network\\Socket\\Win32\\WSABuf.hsc" #-}
    len  <- ((\hsc_ptr -> peekByteOff hsc_ptr 0))  p
{-# LINE 29 "Network\\Socket\\Win32\\WSABuf.hsc" #-}
    return $ WSABuf base len

  poke p iov = do
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p (wsaBufPtr iov)
{-# LINE 33 "Network\\Socket\\Win32\\WSABuf.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p (wsaBufLen iov)
{-# LINE 34 "Network\\Socket\\Win32\\WSABuf.hsc" #-}

-- | @withWSABuf cs f@ executes the computation @f@, passing as argument a pair
-- consisting of a pointer to a temporarily allocated array of pointers to
-- WSABBUF made from @cs@ and the number of pointers (@length cs@).
-- /Windows only/.
withWSABuf :: [(Ptr Word8, Int)] -> ((Ptr WSABuf, Int) -> IO a) -> IO a
withWSABuf [] f = f (nullPtr, 0)
withWSABuf cs f =
    allocaArray csLen $ \aPtr -> do
        zipWithM_ pokeWsaBuf (ptrs aPtr) cs
        f (aPtr, csLen)
  where
    csLen = length cs
    ptrs = iterate (`plusPtr` sizeOf (WSABuf nullPtr 0))
    pokeWsaBuf ptr (sPtr, sLen) = poke ptr $ WSABuf sPtr (fromIntegral sLen)
