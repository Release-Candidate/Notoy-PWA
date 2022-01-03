-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2022 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     Browser.purs
-- Date:     23.Dec.2021
--
-- ==============================================================================
-- | Module Helpers.Browser, helper functions for Browser stuff.
module Helpers.Browser
  ( getCurrentUrl
  , getCurrentUrlString
  , loadFromLocalStorage
  , saveToLocalStorage
  ) where

import Prelude
import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , decodeJson
  , encodeJson
  , fromString
  , stringify
  )
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.StoreKey (class StoreKey, StoreKeyId, key, storeKeyIdToString)
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

{-------------------------------------------------------------------------------
| Serialize the given object as JSON to the local storage.
|
| * `win` - The Browser window to use as context for the local store.
| * `object` - The object to serialize.
-}
saveToLocalStorage ::
  forall a.
  StoreKey a =>
  EncodeJson a => Window -> a -> Effect Unit
saveToLocalStorage win object = do
  s <- localStorage win
  setItem (storeKeyIdToString $ key object) (stringify $ encodeJson object) s

{-------------------------------------------------------------------------------
| Deserialize the given object as JSON from the local storage, using `key` as
| the key.
|
| * `win` - The Browser window to use as context for the local store.
| * `key` - The key in the local storage space to save and retrieve the object.
-}
loadFromLocalStorage ::
  forall a.
  DecodeJson a =>
  Window -> StoreKeyId -> Effect (Maybe a)
loadFromLocalStorage win key = do
  s <- localStorage win
  jsonStr <- getItem (storeKeyIdToString key) s
  case map fromString jsonStr of
    Nothing -> pure Nothing
    Just j -> pure $ hush $ decodeJson j
