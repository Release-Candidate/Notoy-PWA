-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2022 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     NoteComponent.purs
-- Date:     03.Feb.2022
--
-- ==============================================================================
-- | Module App.NoteComponent, the Halogen component for the note.
module App.NoteComponent
  ( Action(..)
  , Output(..)
  , Slot
  , _noteComponent
  , noteComponent
  ) where

import Prelude
import App.Components (newState, newStateAndSave)
import App.ShareTarget (canShare)
import App.State (getState)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Note (KeyWordArray, Note(..), keyWordArrayFromString)
import Data.Show.Generic (genericShow)
import Data.String.Regex (replace)
import Data.String.Regex.Flags (unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.URL (NoteURL, noteUrlFromString, noteUrlToString)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

_noteComponent = Proxy :: Proxy "note"

{-------------------------------------------------------------------------------
| The Halogen slot type of the note component.
|
| Only one instance is allowed, the id is `Unit`.
-}
type Slot
  = forall query. H.Slot query Output Unit

{-------------------------------------------------------------------------------
| All actions the note component sends to the parent.
|
| * Download - Download the current note using the current `Options` state.
| * Geolocation - Do a lookup of the current position, maybe using reverse
|                 geolocation (if enabled in the options).
-}
data Output
  = Download Note
  | Geolocation Note
  | Share Note

data Action
  = Receive Note
  | TitleChanged String
  | URLChanged String
  | PositionChanged String
  | KeywordsChanged String
  | ShortDescChanged String
  | LongDescChanged String
  | GetPosition
  | ShareNote
  | DownloadNote

derive instance eqAction :: Eq Action

derive instance genericAction :: Generic Action _

instance decodeJsonAction :: DecodeJson Action where
  decodeJson = genericDecodeJson

instance encodeJsonAction :: EncodeJson Action where
  encodeJson = genericEncodeJson

instance showAction :: Show Action where
  show = genericShow

noteComponent :: ∀ query m. MonadAff m => H.Component query Note Output m
noteComponent =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , receive = Just <<< Receive
              }
    }
  where
  initialState note = note

{-------------------------------------------------------------------------------
| The note components `Action` (event) handler.
|
| Handles the component's events, by dispatching on event type `action`.
|
| * `action` - The `Action` to process.
-}
handleAction ::
  forall m.
  MonadAff m =>
  Action -> H.HalogenM Note Action () Output m Unit
handleAction action = case action of
  Receive note -> do
    currState <- getState
    case currState == note of
      true -> pure unit
      false -> newStateAndSave newState note
  TitleChanged st -> newStateAndSave newNoteStateTitle st
  URLChanged st -> newStateAndSave (newNoteStateUrl <<< noteUrlFromString) st
  PositionChanged st -> newStateAndSave newNoteStateLocation st
  KeywordsChanged st -> newStateAndSave (newNoteStateKeyWords <<< keyWordArrayFromString) st
  ShortDescChanged st -> newStateAndSave newNoteStateShortDesc st
  LongDescChanged st -> newStateAndSave newNoteStateLongDesc st
  GetPosition -> do
    note <- getState
    H.raise $ Geolocation note
  ShareNote -> do
    note <- getState
    H.raise $ Share note
  DownloadNote -> do
    note <- getState
    H.raise $ Download note

render :: forall m. MonadAff m => Note -> H.ComponentHTML Action () m
render n =
  let
    urlSuffixRegex = unsafeRegex "[/]+$" unicode

    Note note = n
  in
    HH.div [ HP.id "note" ]
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

{-------------------------------------------------------------------------------
| Helper function: set the title string of the `Note` in the state.
|
| To be used with `newStateAndSave`.
-}
newNoteStateTitle ∷
  ∀ action output m.
  MonadAff m =>
  String →
  H.HalogenM Note action () output m Note
newNoteStateTitle newTitle = H.modify \(Note note) -> Note note { title = Just newTitle }

{-------------------------------------------------------------------------------
| Helper function: set the URL of the `Note` in the state.
|
| To be used with `newStateAndSave`.
-}
newNoteStateUrl ∷
  ∀ action output m.
  MonadAff m =>
  Maybe NoteURL →
  H.HalogenM Note action () output m Note
newNoteStateUrl newUrl = H.modify \(Note note) -> Note note { url = newUrl }

{-------------------------------------------------------------------------------
| Helper function: set the keyword array of the `Note` in the state.
|
| To be used with `newStateAndSave`.
-}
newNoteStateKeyWords ∷
  ∀ action output m.
  MonadAff m =>
  Maybe KeyWordArray →
  H.HalogenM Note action () output m Note
newNoteStateKeyWords newKeywords = H.modify \(Note note) -> Note note { keywords = newKeywords }

{-------------------------------------------------------------------------------
| Helper function: set the location string of the `Note` in the state.
|
| To be used with `newStateAndSave`.
-}
newNoteStateLocation ∷
  ∀ action output m.
  MonadAff m =>
  String →
  H.HalogenM Note action () output m Note
newNoteStateLocation newLocation = H.modify \(Note note) -> Note note { location = Just newLocation }

{-------------------------------------------------------------------------------
| Helper function: set the short description string of the `Note` in the state.
|
| To be used with `newStateAndSave`.
-}
newNoteStateShortDesc ∷
  ∀ action output m.
  MonadAff m =>
  String →
  H.HalogenM Note action () output m Note
newNoteStateShortDesc newShortDesc = H.modify \(Note note) -> Note note { shortDesc = Just newShortDesc }

{-------------------------------------------------------------------------------
| Helper function: set the detailed description string of the `Note` in the
| state.
|
| To be used with `newStateAndSave`.
-}
newNoteStateLongDesc ∷
  ∀ action output m.
  MonadAff m =>
  String →
  H.HalogenM Note action () output m Note
newNoteStateLongDesc newLongDesc = H.modify \(Note note) -> Note note { longDesc = Just newLongDesc }