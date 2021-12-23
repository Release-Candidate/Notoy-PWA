-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     Main.purs
-- Date:     23.Dec.2021
--
-- ==============================================================================
module Main
  ( main
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.Event.Internal.Types (Event)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (domcontentloaded)
import Web.HTML.Location (href)
import Web.HTML.Window (location, toEventTarget)
import Web.URL (fromAbsolute, searchParams)
import Web.URL.URLSearchParams (get)

-- | The field names of the share target GET URL.
shareTargetFields ::
  { text :: String -- | Text field. Holding the URL on Android too.
  , title :: String -- | The title field.
  , url :: String -- | The URL field. NOT used on Android.
  }
shareTargetFields = { title: "title", url: "url", text: "text" }

handleShare :: Event -> Effect Unit
handleShare _ = do
  w <- window
  loc <- location w
  urlString <- href loc
  let
    urlUrl = fromAbsolute urlString
  case urlUrl of
    Just u ->
      let
        toSearch = searchParams u

        sharedTitle = get shareTargetFields.title toSearch

        sharedUrl = get shareTargetFields.url toSearch

        sharedText = get shareTargetFields.text toSearch
      in
        case sharedTitle, sharedUrl, sharedText of
          Just shT, Just shU, Just shTx -> do
            log $ "Title: " <> shT <> " URL: " <> shU <> " Text: " <> shTx
          Just shT, Just shU, Nothing -> do
            log $ "Title: " <> shT <> " URL: " <> shU
          Just shT, Nothing, Just shTx -> do
            log $ "Title: " <> shT <> " Text: " <> shTx
          Just shT, Nothing, Nothing -> do
            log $ "Title: " <> shT
          Nothing, Just shU, Just shTx -> do
            log $ " URL: " <> shU <> " Text: " <> shTx
          Nothing, Just shU, Nothing -> do
            log $ " URL: " <> shU
          Nothing, Nothing, Just shTx -> do
            log $ " Text: " <> shTx
          Nothing, Nothing, Nothing -> pure unit
    Nothing -> pure unit

main :: Effect Unit
main = do
  domListener <- eventListener handleShare
  w <- window
  addEventListener domcontentloaded domListener false (toEventTarget w)
