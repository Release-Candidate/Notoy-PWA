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
  ( downloadNote
  , getCurrentUrl
  , getCurrentUrlString
  , getElementFromId
  , getLanguage
  , getLanguages
  , getPlatform
  , isOnline
  , loadFromLocalStorage
  , reverseGeoLocation
  , saveToLocalStorage
  ) where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import App.Constants (downloadAttr, hrefAttr)
import App.Geolocation (GeolocationPosition)
import App.State (State, filenameFromState, makeBlob)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json)
import Data.DateTimeFormat (Locale(..), localeToString)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.StoreKey (class StoreKey, StoreKeyId, storeKeyIdStringFromObject, storeKeyIdToString)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Helpers.DateTime (getDateStringJS)
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
| Request a reverse geolocation of the given position `pos`.
|
| Ues the current default locale of the browser as language of the response.
|
| * `getURLForGeoloc` - A function that takes a `Locale` and the given position
|                       `pos` as arguments and returns the URL of the reverse
|                       geolocation lookup.
| * `geolocJson2String` - A function to convert the JSON geolocation response to
|                         a string to use in the note.
| * `pos` - The `GeolocationPosition` to get the name of.
-}
reverseGeoLocation ::
  (Locale -> GeolocationPosition -> String) ->
  (Json -> String) ->
  GeolocationPosition ->
  Aff (Either String String)
reverseGeoLocation getURLForGeoloc geolocJson2String pos = do
  locale <- liftEffect $ getLanguage unit
  let
    fetchUrl = getURLForGeoloc locale pos
  result <- AX.request (AX.defaultRequest { url = fetchUrl, method = Left GET, responseFormat = ResponseFormat.json })
  case result of
    Left err -> pure $ Left $ AX.printError err
    Right response -> pure $ Right $ geolocJson2String response.body

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
      Just anchorEl -> downloadFromAnchor filenameFromState makeBlob anchorEl state

{-------------------------------------------------------------------------------
| Download the given object from the app.
|
| * `filenameFromObj` - A function to get the filename to use for the download.
| * `makeBlobFromObj` - A function to get the `Blob` of the object to download.
| * `anchorEl` - The hidden anchor HTML element to use for the hidden download
|                link
| * `obj` - The object to download.
-}
downloadFromAnchor ::
  forall a.
  (a -> String) ->
  (a -> String -> String -> Blob) ->
  HTMLA.HTMLAnchorElement -> a -> Effect Unit
downloadFromAnchor filenameFromObj makeBlobFromObj anchorEl obj = do
  let
    filename = filenameFromObj obj

    element = HTMLA.toElement anchorEl
  setAttribute downloadAttr filename element
  HTMLA.setText filename anchorEl
  timestamp <- getDateStringJS unit
  locale <- getLanguage unit
  blobUrl <- createObjectURL $ makeBlobFromObj obj timestamp (localeToString locale)
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
getLanguage :: Unit -> Effect Locale
getLanguage _ = do
  locale <- window >>= navigator >>= language
  pure $ Locale locale

{-------------------------------------------------------------------------------
| Return the array of preferred language of the user / browser.
|
| The first element of this array is `getLanguage`.
-}
getLanguages :: Unit -> Effect (Array Locale)
getLanguages _ = do
  locales <- window >>= navigator >>= languages
  pure $ map Locale locales

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
