-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     Event.purs
-- Date:     23.Dec.2021
--
-- ==============================================================================
module ShareTarget.Event
  ( registerShareEvent
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Note (Note(..))
import Effect (Effect)
import Effect.Console (log)
import Helpers.HTML (getCurrentUrl)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Event.Internal.Types (Event)
import Web.HTML (Window)
import Web.HTML.Event.EventTypes (domcontentloaded)
import Web.HTML.Window (toEventTarget)
import Web.URL (searchParams)
import Web.URL.URLSearchParams (get)

{-
| The field names of the share target GET URL.
|
| * text :: String - Text field. Holding the URL on Android too.
| * title :: String - The title field.
| * url :: String - The URL field. NOT used on Android.
-}
shareTargetFields ::
  { text :: String
  , title :: String
  , url :: String
  }
shareTargetFields = { title: "title", url: "url", text: "text" }

{-
| Event handler for the share event (`domcontentloaded`).
|
| Called, when a website has been shared with this app.
-}
handleShare :: Event -> Effect Unit
handleShare _ = do
  url <- getCurrentUrl unit
  case url of
    Just u ->
      let
        toSearch = searchParams u

        sharedTitle = get shareTargetFields.title toSearch

        sharedUrl = get shareTargetFields.url toSearch

        sharedText = get shareTargetFields.text toSearch

        note = Note { title: sharedTitle, url: sharedUrl, shortDesc: sharedText, longDesc: Just "" }
      in
        log $ show note
    Nothing -> pure unit

{-
| Register the handler for the share event.
|
| * w :: Window - The event target.
-}
registerShareEvent ∷ Window → Effect Unit
registerShareEvent w = do
  domListener <- eventListener handleShare
  addEventListener domcontentloaded domListener false $ toEventTarget w
