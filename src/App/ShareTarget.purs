-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2022 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     ShareTarget.purs
-- Date:     23.Dec.2021
--
-- ==============================================================================
-- | Module App.ShareTarget, code to implement a share target and share notes
-- | with other apps.
module App.ShareTarget
  ( handleShare
  ) where

import Prelude
import App.State (State)
import Data.Maybe (Maybe(..))
import Data.Note (Note(..), fromShared)
import Data.URL (noteUrlFromString)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Helpers.Browser (getCurrentUrl, saveToLocalStorage)
import Web.Event.Internal.Types (Event)
import Web.HTML (Window)
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
handleShare ::
  forall action output m.
  MonadAff m =>
  Window -> Event -> H.HalogenM State action () output m Unit
handleShare win _ = do
  url <- H.liftEffect $ getCurrentUrl unit
  case url of
    Just u -> do
      let
        toSearch = searchParams u

        sharedTitle = get shareTargetFields.title toSearch

        sharedUrl = get shareTargetFields.url toSearch

        maybeURL = noteUrlFromString =<< sharedUrl

        sharedText = get shareTargetFields.text toSearch

        note = fromShared sharedTitle maybeURL sharedText
      case note of
        Note
          { title: Nothing
        , url: Nothing
        , keywords: Nothing
        , shortDesc: Nothing
        , longDesc: Nothing
        } -> pure unit
        _ -> do
          H.liftEffect $ saveToLocalStorage win note
          H.modify_ \state ->
            { note: note
            , options: state.options
            }
          H.liftEffect $ log $ "Got shared note: " <> show note
    Nothing -> pure unit
