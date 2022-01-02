-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     HTML.purs
-- Date:     23.Dec.2021
--
-- =============================================================================
-- | Module Helpers.HTML, helper functions for HTML app stuff.
module Helpers.HTML
  ( getCurrentUrl
  , getCurrentUrlString
  , saveToLocalStorage
  ) where

import Prelude
import Data.Argonaut (class EncodeJson, decodeJson, encodeJson, fromString, jsonParser, stringify)
import Data.Maybe (Maybe)
import Effect (Effect)
import Web.HTML (Window, window)
import Web.HTML.Location (href)
import Web.HTML.Window (localStorage, location)
import Web.Storage.Storage (getItem, setItem)
import Web.URL (URL, fromAbsolute)

{-------------------------------------------------------------------------------
| Return the current URL in the browser's address bar as a `String`.
|
| Needs an `Effect` to get the information from the address bar.
-}
getCurrentUrlString :: Unit -> Effect String
getCurrentUrlString _ = window >>= location >>= href

{-------------------------------------------------------------------------------
| Return the current URL in the browser's address bar as a `Maybe URL`.
|
| Needs an `Effect` to get the information from the address bar.
-}
getCurrentUrl :: Unit -> Effect (Maybe URL)
getCurrentUrl _ = do
  urlString <- getCurrentUrlString unit
  pure $ fromAbsolute urlString

saveToLocalStorage :: forall a. EncodeJson a => Window -> String -> a -> Effect Unit
saveToLocalStorage win key note = do
  s <- localStorage win
  setItem key (stringify $ encodeJson note) s

loadFromLocalStorage win key = do
  s <- localStorage win
  jsonStr <- getItem key s
  pure $ map fromString jsonStr
