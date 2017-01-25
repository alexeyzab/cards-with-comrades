module Routes where

import           Import.NoFoundation

import           AppType

mkYesodData "App" $(parseRoutesFile "config/routes")
