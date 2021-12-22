module Main where

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

        sharedUrl = get "url" toSearch
      in
        case sharedUrl of
          Just str -> do
            log $ "URL: " <> str
          Nothing -> pure unit
    Nothing -> pure unit

main :: Effect Unit
main = do
  domListener <- eventListener handleShare
  w <- window
  addEventListener domcontentloaded domListener false (toEventTarget w)
