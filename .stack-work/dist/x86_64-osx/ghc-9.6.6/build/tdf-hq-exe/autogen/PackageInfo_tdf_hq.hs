{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_tdf_hq (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "tdf_hq"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "TDF HQ - CRM, Scheduling, Packages, Invoicing, Inventory (skeleton)"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
