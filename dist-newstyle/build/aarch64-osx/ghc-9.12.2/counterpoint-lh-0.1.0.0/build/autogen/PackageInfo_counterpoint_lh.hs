{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_counterpoint_lh (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "counterpoint_lh"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Two-voice counterpoint verified by LiquidHaskell refinement types"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
