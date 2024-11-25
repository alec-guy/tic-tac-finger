{-# LINE 1 "Network\\Socket\\Cbits.hsc" #-}
module Network.Socket.Cbits where



import Network.Socket.Imports

-- | This is the value of SOMAXCONN, typically 128.
-- 128 is good enough for normal network servers but
-- is too small for high performance servers.
maxListenQueue :: Int
maxListenQueue = 2147483647
{-# LINE 12 "Network\\Socket\\Cbits.hsc" #-}


{-# LINE 14 "Network\\Socket\\Cbits.hsc" #-}
wsaNotInitialized :: CInt
wsaNotInitialized = 10093
{-# LINE 16 "Network\\Socket\\Cbits.hsc" #-}

{-# LINE 32 "Network\\Socket\\Cbits.hsc" #-}
