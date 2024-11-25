{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_psqueues (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "psqueues"
version :: Version
version = Version [0,2,8,0] []

synopsis :: String
synopsis = "Pure priority search queues"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
