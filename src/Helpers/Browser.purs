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
  ( downloadFromAnchor
  , downloadNote
  , getCurrentUrl
  , getCurrentUrlString
  , getElementFromId
  , getLanguage
  , getLanguages
  , getPlatform
  , isOnline
  , loadFromLocalStorage
  , saveToLocalStorage
  ) where

import Prelude
import App.Constants (downloadAttr, hrefAttr)
import App.State (State, filenameFromState, makeBlob)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.StoreKey (class StoreKey, StoreKeyId, storeKeyIdStringFromObject, storeKeyIdToString)
import Effect (Effect)
import Effect.Console (log)
import Helpers.General (decodeJsonFromString, encodeToJsonString)
import Web.DOM (Element)
import Web.DOM.Element (setAttribute)
import Web.DOM.NonElementParentNode (getElementById)
import Web.File.Blob (Blob)
import Web.File.Url (createObjectURL)
import Web.HTML (Window, window)
import Web.HTML.HTMLAnchorElement as HTMLA
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.HTMLElement (click)
import Web.HTML.Location (href)
import Web.HTML.Navigator (language, languages, onLine, platform)
import Web.HTML.Window (document, localStorage, location, navigator)
import Web.Storage.Storage (getItem, setItem)
import Web.URL (URL, fromAbsolute)

{-------------------------------------------------------------------------------
| Download the given `Note` from the (hidden) anchor element with the given id.
|
| * `aId` - The id of the (hidden) anchor element to use to download.
| * `state` - The state containing the `Options` and `Note` needed to generate
|             the data of the note to download it.
-}
downloadNote :: String -> State -> Effect Unit
downloadNote aId state = do
  hiddenA <- getElementFromId aId
  case hiddenA of
    Nothing -> log $ "Error trying to download: hidden element not found: " <> aId
    Just el -> case HTMLA.fromElement el of
      Nothing ->
        log $ "Error trying to download: the element with id "
          <> aId
          <> " is not an anchor"
      Just anchorEl -> downloadNoteFromAnchor anchorEl state

{-------------------------------------------------------------------------------
| Download the given `Note` from the given (hidden) anchor element.
|
| * `anchorEl` - The (hidden) anchor element to use to download.
| * `state` - The state containing the `Options` and `Note` needed to generate
|             the data of the note to download it.
-}
downloadNoteFromAnchor :: HTMLA.HTMLAnchorElement -> State -> Effect Unit
downloadNoteFromAnchor = downloadFromAnchor filenameFromState makeBlob

{-------------------------------------------------------------------------------
| Download
|
| * `filenameFromObj` -
| * `makeBlobFromObj` -
| * `anchorEl` -
| * `obj` -
-}
downloadFromAnchor ::
  forall a.
  (a -> String) ->
  (a -> Blob) ->
  HTMLA.HTMLAnchorElement -> a -> Effect Unit
downloadFromAnchor filenameFromObj makeBlobFromObj anchorEl obj = do
  let
    filename = filenameFromObj obj

    element = HTMLA.toElement anchorEl
  setAttribute downloadAttr filename element
  HTMLA.setText filename anchorEl
  blobUrl <- createObjectURL $ makeBlobFromObj obj
  setAttribute hrefAttr blobUrl element
  click $ HTMLA.toHTMLElement anchorEl

{-------------------------------------------------------------------------------
| Return the online status of the site.
|
| `true` if we are online, `false` else.
-}
isOnline :: Unit -> Effect Boolean
isOnline _ = window >>= navigator >>= onLine

{-------------------------------------------------------------------------------
| Return the platform (OS) the browser is running on.
-}
getPlatform :: Unit -> Effect String
getPlatform _ = window >>= navigator >>= platform

{-------------------------------------------------------------------------------
| Return the preferred language of the user / browser.
|
| This is the first element of the array `getLanguages`.
-}
getLanguage :: Unit -> Effect String
getLanguage _ = window >>= navigator >>= language

{-------------------------------------------------------------------------------
| Return the array of preferred language of the user / browser.
|
| The first element of this array is `getLanguage`.
-}
getLanguages :: Unit -> Effect (Array String)
getLanguages _ = window >>= navigator >>= languages

{-------------------------------------------------------------------------------
| Return the HTML element with the given id (if such an element exists).
|
| * `id` - The element id to search for.
-}
getElementFromId :: String -> Effect (Maybe Element)
getElementFromId id = do
  doc <- document =<< window
  getElementById id $ toNonElementParentNode doc

{-------------------------------------------------------------------------------
| Return the current URL in the browser's address bar as a `String`.
-}
getCurrentUrlString :: Unit -> Effect String
getCurrentUrlString _ = window >>= location >>= href

{-------------------------------------------------------------------------------
| Return the current URL in the browser's address bar as a `Maybe URL`.
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
  setItem (storeKeyIdStringFromObject object) (encodeToJsonString object) s

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
  case jsonStr of
    Nothing -> do
      log $ "Error loading item for key " <> storeKeyIdToString key
      pure Nothing
    Just str -> case decodeJsonFromString str of
      Left e -> do
        log e
        pure Nothing
      Right obj -> pure obj
