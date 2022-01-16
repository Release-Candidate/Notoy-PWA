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
import App.Action (Action(..), handleAction)
import App.Constants (appElementId)
import App.ShareTarget (canShare)
import App.State (State, initialState)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Note (Note(..))
import Data.String.Regex (replace)
import Data.String.Regex.Flags (unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.URL (noteUrlToString)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Partial.Unsafe (unsafePartial)
import Web.DOM.ParentNode (QuerySelector(..))

-- | Main entry point of the app.
main :: Effect Unit
main =
  HA.runHalogenAff do
    HA.awaitLoad
    appEl <- HA.selectElement (QuerySelector appElementId)
    let
      app = unsafePartial $ fromJust appEl
    runUI component unit app

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
  let
    urlSuffixRegex = unsafeRegex "[/]+$" unicode

    Note
      { title: noteTitle
    , url: noteURL
    , keywords: noteKeywords
    , shortDesc: noteShortDesc
    , longDesc: noteLongDesc
    } = state.note
  in
    HH.div_
      [ HH.div [ HP.id "title" ]
          [ HH.label [ HP.for "titleText" ]
              [ HH.span_ [ HH.text "Title:" ]
              , HH.br_
              , HH.input
                  [ HP.id "titleText"
                  , HP.type_ HP.InputText
                  , HP.min 50.0
                  , HP.value $ fromMaybe "" noteTitle
                  , HE.onValueInput \st -> TitleChanged st
                  ]
              ]
          ]
      , HH.div [ HP.id "url" ]
          [ HH.label [ HP.for "pageURL" ]
              [ HH.span_ [ HH.text "URL:" ]
              , HH.br_
              , HH.input
                  [ HP.id "pageURL"
                  , HP.type_ HP.InputUrl
                  , HP.min 50.0
                  , HP.value $ replace urlSuffixRegex "" $ fromMaybe "" $ map noteUrlToString noteURL
                  , HE.onValueInput \st -> URLChanged st
                  ]
              ]
          ]
      , HH.div [ HP.id "keywords" ]
          [ HH.label [ HP.for "keyWords" ]
              [ HH.span_ [ HH.text "Keywords (comma separated):" ]
              , HH.br_
              , HH.input
                  [ HP.id "keyWords"
                  , HP.type_ HP.InputText
                  , HP.min 50.0
                  , HP.placeholder "keyword1, key word 2, Keyword 3"
                  , HP.value $ fromMaybe "" $ map show noteKeywords
                  , HE.onValueInput \st -> KeywordsChanged st
                  ]
              ]
          ]
      , HH.div [ HP.id "description" ]
          [ HH.label [ HP.for "descriptionText" ]
              [ HH.span_ [ HH.text "Short description:" ]
              , HH.br_
              , HH.textarea
                  [ HP.id "descriptionText"
                  , HP.rows 5
                  , HP.cols 60
                  , HP.value $ fromMaybe "" noteShortDesc
                  , HE.onValueInput \st -> ShortDescChanged st
                  ]
              ]
          ]
      , HH.div [ HP.id "detailed_text" ]
          [ HH.label [ HP.for "detailedDescription" ]
              [ HH.span_ [ HH.text "Detailed description:" ]
              , HH.br_
              , HH.textarea
                  [ HP.id "detailedDescription"
                  , HP.rows 5
                  , HP.cols 60
                  , HP.value $ fromMaybe "" noteLongDesc
                  , HE.onValueInput \st -> LongDescChanged st
                  ]
              ]
          ]
      , HH.div [ HP.id "save" ]
          [ HH.button
              [ HP.id "saveButton"
              , HE.onClick \_ -> DownloadNote
              ]
              [ HH.text "Save" ]
          ]
      , HH.div [ HP.id "share" ]
          if canShare unit then
            [ HH.button
                [ HP.id "shareButton"
                , HE.onClick \_ -> ShareNote
                ]
                [ HH.text "Share" ]
            ]
          else
            []
      ]
