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
import Data.Options (AddDate(..), AddYamlHeader(..), Format(..), LookupLocation(..), Options(..))
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
    appEl <- HA.selectElement (QuerySelector $ "#" <> appElementId)
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

    Note note = state.note

    Options options = state.options
  in
    HH.div [ HP.id "all" ]
      [ HH.div [ HP.id "hiddenDiv" ]
          [ HH.a [ HP.id "hiddenURL", hiddenP true ]
              [ HH.text "download.md" ]
          ]
      , HH.div [ HP.id "note" ]
          [ HH.div [ HP.id "title" ]
              [ HH.label [ HP.for "titleText" ]
                  [ HH.span_ [ HH.text "Title:" ]
                  , HH.br_
                  , HH.input
                      [ HP.id "titleText"
                      , HP.type_ HP.InputText
                      , HP.min 50.0
                      , HP.value $ fromMaybe "" note.title
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
                      , HP.value $ replace urlSuffixRegex ""
                          $ fromMaybe ""
                          $ map noteUrlToString note.url
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
                      , HP.value $ fromMaybe "" $ map show note.keywords
                      , HE.onValueInput \st -> KeywordsChanged st
                      ]
                  ]
              ]
          , HH.div [ HP.id "Geolocation" ]
              [ HH.label [ HP.for "currentPosition" ]
                  [ HH.span_ [ HH.text "Location:" ]
                  , HH.br_
                  , HH.input
                      [ HP.id "currentPosition"
                      , HP.type_ HP.InputText
                      , HP.min 50.0
                      , HP.placeholder "Position"
                      , HP.value $ fromMaybe "" note.location
                      , HE.onValueInput \st -> PositionChanged st
                      ]
                  ]
              , HH.button
                  [ HP.id "positionButton"
                  , HE.onClick \_ -> GetPosition
                  ]
                  [ HH.text "Get current position" ]
              ]
          , HH.div [ HP.id "description" ]
              [ HH.label [ HP.for "descriptionText" ]
                  [ HH.span_ [ HH.text "Short description:" ]
                  , HH.br_
                  , HH.textarea
                      [ HP.id "descriptionText"
                      , HP.rows 5
                      , HP.cols 60
                      , HP.value $ fromMaybe "" note.shortDesc
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
                      , HP.value $ fromMaybe "" note.longDesc
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
              if canShare then
                [ HH.button
                    [ HP.id "shareButton"
                    , HE.onClick \_ -> ShareNote
                    ]
                    [ HH.text "Share" ]
                ]
              else
                []
          ]
      , HH.div [ HP.id "options" ]
          [ HH.div [ HP.id "format" ]
              [ HH.fieldset
                  [ HP.id "formatParent" ]
                  [ HH.legend [] [ HH.text "Note file format" ]
                  , HH.label [ HP.for "markdown" ]
                      [ HH.input
                          [ HP.id "markdown"
                          , HP.name "formatRadio"
                          , HP.type_ HP.InputRadio
                          , HP.value $ show Markdown
                          , HP.checked $ options.format == Markdown
                          , HE.onChecked \_ -> FormatChanged $ show Markdown
                          ]
                      , HH.text "Markdown (Obsidian, Joplin, Zettlr)"
                      ]
                  , HH.label [ HP.for "orgMode" ]
                      [ HH.input
                          [ HP.id "orgMode"
                          , HP.name "formatRadio"
                          , HP.type_ HP.InputRadio
                          , HP.value $ show OrgMode
                          , HP.checked $ options.format == OrgMode
                          , HE.onChecked \_ -> FormatChanged $ show OrgMode
                          ]
                      , HH.text "Org-Mode (Emacs)"
                      ]
                  , HH.label [ HP.for "text" ]
                      [ HH.input
                          [ HP.id "text"
                          , HP.name "formatRadio"
                          , HP.type_ HP.InputRadio
                          , HP.value $ show Text
                          , HP.checked $ options.format == Text
                          , HE.onChecked \_ -> FormatChanged $ show Text
                          ]
                      , HH.text "Plain Text"
                      ]
                  ]
              ]
          ]
      , HH.div [ HP.id "timestamp" ]
          [ HH.label [ HP.for "timestampInput" ]
              [ HH.span_ [ HH.text "Add the current date to the note?" ]
              , HH.input
                  [ HP.type_ HP.InputCheckbox
                  , HP.id "timestampInput"
                  , HP.checked $ options.addDate == AddDate
                  , HE.onChecked \b -> AddDateChanged b
                  ]
              ]
          ]
      , HH.div [ HP.id "reverseGeolocation" ]
          [ HH.label [ HP.for "reverseGeolocationInput" ]
              [ HH.span_ [ HH.text "Look the position up on BigData?" ]
              , HH.input
                  [ HP.type_ HP.InputCheckbox
                  , HP.id "reverseGeolocationInput"
                  , HP.checked $ options.lookupLocation == ReverseGeolocation
                  , HE.onChecked \b -> ReverseGeolocChanged b
                  ]
              ]
          ]
      , HH.div [ HP.id "yaml" ]
          [ HH.label [ HP.for "yamlFrontMatter" ]
              [ HH.span_ [ HH.text "Add YAML front matter (YAML metadata block for Pandoc)?" ]
              , HH.input
                  [ HP.type_ HP.InputCheckbox
                  , HP.id "yamlFrontMatter"
                  , HP.checked $ options.addYaml == AddYamlHeader
                  , HE.onChecked \b -> AddYamlHeaderChanged b
                  ]
              ]
          ]
      ]

{-------------------------------------------------------------------------------
| HTML attribute `hidden`, to hide elements.
-}
hiddenP :: forall r i. Boolean -> HH.IProp ( hidden :: Boolean | r ) i
hiddenP = HH.prop (HH.PropName "hidden")
