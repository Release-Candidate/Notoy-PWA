-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2022 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     Constants.purs
-- Date:     06.Jan.2022
--
-- =============================================================================
-- | Module App.Constants, various constants used in the app.
module App.Constants
  ( appElementId
  , downloadAttr
  , hiddenURLId
  , hrefAttr
  ) where

downloadAttr :: String
downloadAttr = "download"

hrefAttr :: String
hrefAttr = "href"

{-------------------------------------------------------------------------------
| The id of the HTML div to render the app to.
|
| "app"
-}
appElementId :: String
appElementId = "app"

{-------------------------------------------------------------------------------
| The id of the hidden `a` HTML element used to download the note.
|
| "hiddenURL"
-}
hiddenURLId ∷ String
hiddenURLId = "hiddenURL"
