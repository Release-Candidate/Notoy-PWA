-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     Event.purs
-- Date:     23.Dec.2021
--
-- =============================================================================
module ShareTarget.Event
  ( handleShare
  , registerShareEvent
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Note (fromShared)
import Data.URL (noteUrlFromString)
import Effect (Effect)
import Halogen as H
import Effect.Console (log)
import Helpers.Browser (getCurrentUrl, saveToLocalStorage)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Event.Internal.Types (Event)
import Web.HTML (Window)
import Web.HTML.Event.EventTypes (domcontentloaded)
import Web.HTML.Window (toEventTarget)
import Web.URL (searchParams)
import Web.URL.URLSearchParams (get)

{-------------------------------------------------------------------------------
| The field names of the share target GET URL.
|
| * text :: String - Text field. The URL on Android.
| * title :: String - The title field.
| * url :: String - The URL field. NOT used on Android.
-}
shareTargetFields ::
  { text :: String
  , title :: String
  , url :: String
  }
shareTargetFields = { title: "title", url: "url", text: "text" }

{-------------------------------------------------------------------------------
| Event handler for the share event (`domcontentloaded`).
|
| Called, when a website has been shared with this app.
|
| On Android, the data of the shared URL is as follows:
| Title: Hacker News
| URL is empty.
| Text: https://news.ycombinator.com/news
-}
handleShare :: Window -> Event -> Effect Unit
handleShare win _ = do
  url <- getCurrentUrl unit
  case url of
    Just u -> do
      let
        toSearch = searchParams u

        sharedTitle = get shareTargetFields.title toSearch

        sharedUrl = get shareTargetFields.url toSearch

        maybeURL = noteUrlFromString =<< sharedUrl

        sharedText = get shareTargetFields.text toSearch

        note = fromShared sharedTitle maybeURL sharedText
      saveToLocalStorage win note
      log $ show note
    Nothing -> pure unit

{-------------------------------------------------------------------------------
| Register the handler for the share event.
|
| * w :: Window - The event target.
-}
registerShareEvent ∷ Window → Effect Unit
registerShareEvent w = do
  domListener <- eventListener $ handleShare w
  addEventListener domcontentloaded domListener false $ toEventTarget w
