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
  , component
  ) where

import Prelude
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
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Helpers.Components (modifyComponentStateAndSave, setState)
import Type.Proxy (Proxy(..))

{-------------------------------------------------------------------------------
| Proxy to use in the slot of the parent component.
-}
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
| * Share - Share the note with other apps.
-}
data Output
  = Download Note
  | Geolocation Note
  | Share Note

{-------------------------------------------------------------------------------
| The actions (events) of a note component.
|
| * Receive - Input from the parent component received.
| * TitleChanged - The note's title has changed.
| * URLChanged - The note's URL has changed.
| * PositionChanged - The note's position has changed.
| * KeywordsChanged - The note's keywords has changed.
| * ShortDescChanged - The note's short description has changed.
| * LongDescChanged - The note's detailed description has changed.
| * GetPosition - Get the current position of the device.
| * ShareNote - Share the note with another app.
| * DownloadNote - Download the note.
-}
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

{-------------------------------------------------------------------------------
| The `NoteComponent`'s description for use in the parent component.
-}
component :: ∀ query m. MonadAff m => H.Component query Note Output m
component =
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
      false -> modifyComponentStateAndSave setState note
  TitleChanged st -> modifyComponentStateAndSave setNoteStateTitle st
  URLChanged st -> modifyComponentStateAndSave (setNoteStateUrl <<< noteUrlFromString) st
  PositionChanged st -> modifyComponentStateAndSave setNoteStateLocation st
  KeywordsChanged st -> modifyComponentStateAndSave (setNoteStateKeyWords <<< keyWordArrayFromString) st
  ShortDescChanged st -> modifyComponentStateAndSave setNoteStateShortDesc st
  LongDescChanged st -> modifyComponentStateAndSave setNoteStateLongDesc st
  GetPosition -> do
    note <- getState
    H.raise $ Geolocation note
  ShareNote -> do
    note <- getState
    H.raise $ Share note
  DownloadNote -> do
    note <- getState
    H.raise $ Download note

{-------------------------------------------------------------------------------
| Halogen's render function of this component.
-}
render :: forall m. MonadAff m => Note -> H.ComponentHTML Action () m
render n =
  let
    urlSuffixRegex = unsafeRegex "[/]+$" unicode

    Note note = n
  in
    HH.div
      [ HP.id "note"
      , HP.classes
          [ ClassName "container"
          , ClassName "mx-auto"
          , ClassName "space-y-3"
          ]
      ]
      [ HH.div [ HP.id "title", HP.classes [ ClassName "block" ] ]
          [ HH.label [ HP.for "titleText" ]
              [ HH.span [ HP.classes [] ] [ HH.text "Title:" ]
              , HH.br_
              , HH.input
                  [ HP.id "titleText"
                  , HP.classes
                      [ ClassName "rounded-md"
                      , ClassName "border-gray-300"
                      , ClassName "shadow-sm"
                      , ClassName "w-full"
                      ]
                  , HP.type_ HP.InputText
                  , HP.min 50.0
                  , HP.value $ fromMaybe "" note.title
                  , HE.onValueInput \st -> TitleChanged st
                  ]
              ]
          ]
      , HH.div [ HP.id "url", HP.classes [ ClassName "block" ] ]
          [ HH.label [ HP.for "pageURL" ]
              [ HH.span_ [ HH.text "URL:" ]
              , HH.br_
              , HH.input
                  [ HP.id "pageURL"
                  , HP.classes
                      [ ClassName "rounded-md"
                      , ClassName "border-gray-300"
                      , ClassName "shadow-sm"
                      , ClassName "w-full"
                      ]
                  , HP.type_ HP.InputUrl
                  , HP.min 50.0
                  , HP.value $ replace urlSuffixRegex ""
                      $ fromMaybe ""
                      $ map noteUrlToString note.url
                  , HE.onValueInput \st -> URLChanged st
                  ]
              ]
          ]
      , HH.div [ HP.id "keywords", HP.classes [ ClassName "block" ] ]
          [ HH.label [ HP.for "keyWords" ]
              [ HH.span_ [ HH.text "Keywords (comma separated):" ]
              , HH.br_
              , HH.input
                  [ HP.id "keyWords"
                  , HP.classes
                      [ ClassName "rounded-md"
                      , ClassName "border-gray-300"
                      , ClassName "shadow-sm"
                      , ClassName "w-full"
                      ]
                  , HP.type_ HP.InputText
                  , HP.min 50.0
                  , HP.placeholder "keyword1, key word 2, Keyword 3"
                  , HP.value $ fromMaybe "" $ map show note.keywords
                  , HE.onValueInput \st -> KeywordsChanged st
                  ]
              ]
          ]
      , HH.div
          [ HP.id "Geolocation"
          , HP.classes
              [ ClassName "flex"
              , ClassName "flex-row"
              , ClassName "flex-wrap"
              , ClassName "items-end"
              , ClassName "space-x-4"
              , ClassName "space-y-2"
              ]
          ]
          [ HH.label
              [ HP.for "currentPosition"
              , HP.classes [ ClassName "inline", ClassName "grow" ]
              ]
              [ HH.span_ [ HH.text "Location:" ]
              , HH.br_
              , HH.input
                  [ HP.id "currentPosition"
                  , HP.classes
                      [ ClassName "rounded-md"
                      , ClassName "border-gray-300"
                      , ClassName "shadow-sm"
                      , ClassName "w-full"
                      ]
                  , HP.type_ HP.InputText
                  , HP.min 50.0
                  , HP.placeholder "Position"
                  , HP.value $ fromMaybe "" note.location
                  , HE.onValueInput \st -> PositionChanged st
                  ]
              ]
          , HH.button
              [ HP.id "positionButton"
              , HP.classes
                  [ ClassName "inline"
                  , ClassName "btn"
                  , ClassName "btn-blue"
                  , ClassName "position"
                  , ClassName "h-fit-content"
                  ]
              , HE.onClick \_ -> GetPosition
              ]
              [ HH.text "Get position" ]
          ]
      , HH.div [ HP.id "description", HP.classes [ ClassName "block" ] ]
          [ HH.label [ HP.for "descriptionText" ]
              [ HH.span_ [ HH.text "Short description:" ]
              , HH.br_
              , HH.textarea
                  [ HP.id "descriptionText"
                  , HP.classes
                      [ ClassName "rounded-md"
                      , ClassName "border-gray-300"
                      , ClassName "shadow-sm"
                      , ClassName "resize"
                      , ClassName "w-full"
                      ]
                  , HP.rows 5
                  , HP.cols 40
                  , HP.value $ fromMaybe "" note.shortDesc
                  , HE.onValueInput \st -> ShortDescChanged st
                  ]
              ]
          ]
      , HH.div [ HP.id "detailed_text", HP.classes [ ClassName "block" ] ]
          [ HH.label [ HP.for "detailedDescription" ]
              [ HH.span_ [ HH.text "Detailed description:" ]
              , HH.br_
              , HH.textarea
                  [ HP.id "detailedDescription"
                  , HP.classes
                      [ ClassName "rounded-md"
                      , ClassName "border-gray-300"
                      , ClassName "shadow-sm"
                      , ClassName "resize"
                      , ClassName "w-full"
                      ]
                  , HP.rows 5
                  , HP.cols 40
                  , HP.value $ fromMaybe "" note.longDesc
                  , HE.onValueInput \st -> LongDescChanged st
                  ]
              ]
          ]
      , HH.div [ HP.id "buttons", HP.classes [ ClassName "space-x-4" ] ]
          [ HH.div
              [ HP.id "save"
              , HP.classes
                  [ ClassName "inline"
                  , ClassName "px-2"
                  ]
              ]
              [ HH.button
                  [ HP.id "saveButton"
                  , HP.classes
                      [ ClassName "btn"
                      , ClassName "btn-blue"
                      , ClassName "download"
                      ]
                  , HE.onClick \_ -> DownloadNote
                  ]
                  [ HH.text "Save" ]
              ]
          , HH.div
              [ HP.id "share"
              , HP.classes
                  [ ClassName "inline"
                  , ClassName "px-2"
                  ]
              ]
              if canShare then
                [ HH.button
                    [ HP.id "shareButton"
                    , HP.classes
                        [ ClassName "btn"
                        , ClassName "btn-blue"
                        , ClassName "share"
                        ]
                    , HE.onClick \_ -> ShareNote
                    ]
                    [ HH.text "Share" ]
                ]
              else
                []
          ]
      ]

{-------------------------------------------------------------------------------
| Helper function: set the title string of the `Note` in the state.
|
| To be used with `modifyComponentStateAndSave`.
-}
setNoteStateTitle ∷
  ∀ action output m.
  MonadAff m =>
  String →
  H.HalogenM Note action () output m Note
setNoteStateTitle newTitle = H.modify \(Note note) -> Note note { title = Just newTitle }

{-------------------------------------------------------------------------------
| Helper function: set the URL of the `Note` in the state.
|
| To be used with `modifyComponentStateAndSave`.
-}
setNoteStateUrl ∷
  ∀ action output m.
  MonadAff m =>
  Maybe NoteURL →
  H.HalogenM Note action () output m Note
setNoteStateUrl newUrl = H.modify \(Note note) -> Note note { url = newUrl }

{-------------------------------------------------------------------------------
| Helper function: set the keyword array of the `Note` in the state.
|
| To be used with `modifyComponentStateAndSave`.
-}
setNoteStateKeyWords ∷
  ∀ action output m.
  MonadAff m =>
  Maybe KeyWordArray →
  H.HalogenM Note action () output m Note
setNoteStateKeyWords newKeywords = H.modify \(Note note) -> Note note { keywords = newKeywords }

{-------------------------------------------------------------------------------
| Helper function: set the location string of the `Note` in the state.
|
| To be used with `modifyComponentStateAndSave`.
-}
setNoteStateLocation ∷
  ∀ action output m.
  MonadAff m =>
  String →
  H.HalogenM Note action () output m Note
setNoteStateLocation newLocation = H.modify \(Note note) -> Note note { location = Just newLocation }

{-------------------------------------------------------------------------------
| Helper function: set the short description string of the `Note` in the state.
|
| To be used with `modifyComponentStateAndSave`.
-}
setNoteStateShortDesc ∷
  ∀ action output m.
  MonadAff m =>
  String →
  H.HalogenM Note action () output m Note
setNoteStateShortDesc newShortDesc = H.modify \(Note note) -> Note note { shortDesc = Just newShortDesc }

{-------------------------------------------------------------------------------
| Helper function: set the detailed description string of the `Note` in the
| state.
|
| To be used with `modifyComponentStateAndSave`.
-}
setNoteStateLongDesc ∷
  ∀ action output m.
  MonadAff m =>
  String →
  H.HalogenM Note action () output m Note
setNoteStateLongDesc newLongDesc = H.modify \(Note note) -> Note note { longDesc = Just newLongDesc }
