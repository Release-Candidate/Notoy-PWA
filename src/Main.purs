-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     Main.purs
-- Date:     23.Dec.2021
--
-- =============================================================================
-- | # Module Main
-- |
-- | Main entry point of the app.
module Main
  ( main
  ) where

import Prelude
import App.Constants (appElementId)
import Data.Maybe (Maybe(..), fromJust)
import Data.Note (Note(..), defaultNote, fromShared, noteKeyId)
import Data.Options (Options, defaultOptions)
import Data.URL (noteUrlFromString)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Query.Event (eventListener)
import Halogen.VDom.Driver (runUI)
import Helpers.Browser (getCurrentUrl, loadFromLocalStorage, saveToLocalStorage)
import Partial.Unsafe (unsafePartial)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Internal.Types (Event)
import Web.HTML (Window, window)
import Web.HTML.Event.EventTypes (domcontentloaded)
import Web.HTML.Window (toEventTarget)
import Web.URL (searchParams)
import Web.URL.URLSearchParams (get)

-- | Main entry point of the app.
main :: Effect Unit
main =
  HA.runHalogenAff do
    HA.awaitLoad
    appEl <- HA.selectElement (QuerySelector appElementId)
    let
      app = unsafePartial $ fromJust appEl
    runUI component unit app

{-------------------------------------------------------------------------------
| The App's state.
-}
type State
  = { options :: Options
    , note :: Note
    }

initialState :: forall input. input -> State
initialState _ =
  { options: defaultOptions
  , note: defaultNote
  }

{-------------------------------------------------------------------------------
| The app's actions, called if an event occurred.
-}
data Action
  = Initialize
  | ShareEvent Event

component ::
  forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.p_ [ HH.text "Just some text, so that this area is filled ..." ] ]

handleAction ::
  forall output m.
  MonadAff m =>
  Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> do
    win <- H.liftEffect $ window
    (loadedNote :: Maybe Note) <- H.liftEffect $ loadFromLocalStorage win noteKeyId
    case loadedNote of
      Nothing -> H.liftEffect $ log $ "No Note loaded!"
      Just savedNote -> do
        currState <-
          H.modify \state ->
            { note: savedNote
            , options: state.options
            }
        H.liftEffect $ log $ "Loaded Note: " <> show currState.note
    H.subscribe' \_ ->
      eventListener
        domcontentloaded
        (toEventTarget win)
        (\e -> Just $ ShareEvent e)
  ShareEvent e -> do
    win <- H.liftEffect $ window
    handleShare win e

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
  forall output m.
  MonadAff m =>
  Window -> Event -> H.HalogenM State Action () output m Unit
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
        Note { title: Nothing, url: Nothing, keywords: Nothing, shortDesc: Nothing, longDesc: Nothing } -> pure unit
        _ -> do
          H.liftEffect $ saveToLocalStorage win note
          H.modify_ \state ->
            { note: note
            , options: state.options
            }
          H.liftEffect $ log $ "Got shared note: " <> show note
    Nothing -> pure unit
