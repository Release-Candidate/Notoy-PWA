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
  ( canShare
  , handleShare
  , shareNote
  ) where

import Prelude
import App.State (State, newNoteState_)
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Note (Note(..), fromShared)
import Data.URL (noteUrlFromString, noteUrlToString)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Helpers.Browser (getCurrentUrl, saveToLocalStorage)
import Web.Event.Internal.Types (Event)
import Web.HTML (Window)
import Web.URL (searchParams)
import Web.URL.URLSearchParams (get)

{-------------------------------------------------------------------------------
| Return `true`, if the platform supports sharing the current note to other
| apps, `false` if not.
-}
canShare :: Boolean
canShare = canShareJS unit

{-------------------------------------------------------------------------------
| Share the given Note `note` with other apps.
|
| * `note` - The Note to share.
-}
shareNote :: Note -> Aff Unit
shareNote note =
  if canShare then
    toAffE $ shareNoteJS noteRecord
  else
    pure unit
  where
  Note
    { title: title
  , url: url
  , keywords: keywords
  , shortDesc: shortDesc
  , longDesc: longDesc
  } = note

  keywordString = case keywords of
    Nothing -> ""
    Just keywordArr -> "Keywords: " <> show keywordArr <> "\n\n"

  shortDescString = case shortDesc of
    Nothing -> ""
    Just shortD -> shortD <> "\n"

  noteRecord =
    { title: fromMaybe "" title
    , url: fromMaybe "" $ map noteUrlToString url
    , text: keywordString <> shortDescString <> fromMaybe "" longDesc
    }

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
  forall action output m row.
  MonadAff m =>
  Window -> Event -> H.HalogenM State action (row) output m Unit
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
          newNoteState_ note
          H.liftEffect $ saveToLocalStorage win note
          H.liftEffect $ log $ "Got shared note: " <> show note
    Nothing -> pure unit

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
| Record type for interop with Javascript, especially `shareNoteJS`.
-}
type ShareTargetRecord
  = { title :: String
    , text :: String
    , url :: String
    }

{-------------------------------------------------------------------------------
| Import of JS function `canShareJS` from `ShareTarget.js`.
-}
foreign import canShareJS :: Unit -> Boolean

{-------------------------------------------------------------------------------
| Import of JS function `shareNoteJS` from `ShareTarget.js`.
-}
foreign import shareNoteJS :: ShareTargetRecord -> Effect (Promise Unit)
