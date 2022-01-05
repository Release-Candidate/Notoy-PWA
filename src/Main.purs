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
import Data.Maybe (Maybe(..))
import Data.Note (Note(..), fromShared)
import Data.Options (AddDate(..), AddYamlHeader(..), Format(..), Options(..))
import Data.URL (noteUrlFromString)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen.HTML as HH
import Halogen.Query.Event (eventListener)
import Helpers.Browser (getCurrentUrl, saveToLocalStorage)
import Web.DOM.Node (nodeName)
import Web.DOM.ParentNode (QuerySelector(..))
import Web.Event.Internal.Types (Event)
import Web.HTML (Window, window)
import Web.HTML.Event.EventTypes (domcontentloaded)
import Web.HTML.Window (toEventTarget)
import Web.URL (searchParams)
import Web.URL.URLSearchParams (get)

{-------------------------------------------------------------------------------
| The id of the HTML div to render the app to.
|
| "#app"
-}
appElementId :: String
appElementId = "#app"

{-------------------------------------------------------------------------------
| The App's state.
-}
type State
  = { options :: Options
    , note :: Note
    }

initialState :: forall input. input -> State
initialState _ =
  { options:
      Options
        { format: Markdown
        , addDate: AddDate
        , addYaml: AddYamlHeader
        }
  , note:
      Note
        { title: Nothing
        , url: Nothing
        , keywords: Nothing
        , shortDesc: Nothing
        , longDesc: Nothing
        }
  }

{-------------------------------------------------------------------------------
| The app's actions, called if an event occurred.
-}
data Action
  = Initialize
  | ShareEvent Event

component1 ::
  forall query input output m. MonadAff m => H.Component query input output m
component1 =
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
      H.liftEffect $ saveToLocalStorage win note
      H.liftEffect $ log $ show note
      pure unit
    Nothing -> pure unit

-- | Main entry point of the app.
main :: Effect Unit
main =
  HA.runHalogenAff do
    HA.awaitLoad
    appEl <- HA.selectElement (QuerySelector appElementId)
    case appEl of
      Nothing -> do
        body <- HA.awaitBody
        runUI component1 unit body
      Just app -> runUI component1 unit app
