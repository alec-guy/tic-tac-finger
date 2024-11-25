{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_network (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "network"
version :: Version
version = Version [3,1,4,0] []

synopsis :: String
synopsis = "Low-level networking interface"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/haskell/network"
