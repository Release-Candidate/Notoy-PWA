-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2022 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     Action.purs
-- Date:     16.Jan.2022
--
-- ==============================================================================
-- | Module App.Action, defines the app's actions.
module App.Action
  ( Action(..)
  , handleAction
  ) where

import Prelude
import App.BigDataGeoLoc (bigDataGeolocResponse, bigDataGeolocURL)
import App.Constants (hiddenURLId)
import App.Geolocation (defaultGeoLocOptions, getCurrentPosition, setTimeout)
import App.ShareTarget (handleShare, shareNote)
import App.State
  ( State
  , getState
  , newNoteState
  , newNoteStateKeyWords
  , newNoteStateLongDesc
  , newNoteStateShortDesc
  , newNoteStateTitle
  , newNoteStateUrl
  , newOptionsState
  , newOptionsStateAddDate
  , newOptionsStateAddYamlHeader
  , newOptionsStateFormat
  )
import Data.Argonaut (class DecodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Note (keyWordArrayFromString, noteKeyId)
import Data.Options
  ( addDateFromBool
  , formatFromString
  , optionsKeyId
  , yamlHeaderFromBool
  )
import Data.StoreKey (class StoreKey, StoreKeyId)
import Data.Time.Duration (Milliseconds(..))
import Data.URL (noteUrlFromString)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.Query.Event (eventListener)
import Helpers.Browser
  ( downloadNote
  , loadFromLocalStorage
  , reverseGeoLocation
  , saveToLocalStorage
  )
import Partial.Unsafe (unsafePartial)
import Web.Event.Internal.Types (Event)
import Web.HTML (Window, window)
import Web.HTML.Event.EventTypes (domcontentloaded)
import Web.HTML.Window (toEventTarget)

{-------------------------------------------------------------------------------
| The app's actions, emitted if an event has occurred.
-}
data Action
  = Initialize
  | ShareTargetEvent Event
  | TitleChanged String
  | URLChanged String
  | GetPosition
  | KeywordsChanged String
  | ShortDescChanged String
  | LongDescChanged String
  | FormatChanged String
  | AddDateChanged Boolean
  | AddYamlHeaderChanged Boolean
  | ShareNote
  | DownloadNote

{-------------------------------------------------------------------------------
| The app's main `Action` (event) handler.
|
| Handles the app's events, by dispatching on event type `e`.
|
| * `e` - The `Action` to process.
-}
handleAction ::
  forall output m.
  MonadAff m =>
  Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Initialize -> appInit
  ShareTargetEvent e -> do
    win <- H.liftEffect window
    handleShare win e
  TitleChanged st -> newStateAndSave newNoteStateTitle st
  URLChanged st -> newStateAndSave (newNoteStateUrl <<< noteUrlFromString) st
  GetPosition -> getPosition
  KeywordsChanged st -> newStateAndSave (newNoteStateKeyWords <<< keyWordArrayFromString) st
  ShortDescChanged st -> newStateAndSave newNoteStateShortDesc st
  LongDescChanged st -> newStateAndSave newNoteStateLongDesc st
  FormatChanged st -> newStateAndSave (newOptionsStateFormat <<< formatFromString) st
  AddDateChanged b -> newStateAndSave (newOptionsStateAddDate <<< addDateFromBool) b
  AddYamlHeaderChanged b -> newStateAndSave (newOptionsStateAddYamlHeader <<< yamlHeaderFromBool) b
  ShareNote -> do
    state <- getState
    H.liftAff $ shareNote state.note
  DownloadNote -> do
    state <- getState
    H.liftEffect $ downloadNote hiddenURLId state

{-------------------------------------------------------------------------------
| Initialization.
|
| called on initialization of the app.
-}
appInit :: forall output m. MonadAff m => H.HalogenM State Action () output m Unit
appInit = do
  win <- H.liftEffect $ window
  loadOptions win
  loadNote win
  H.subscribe' \_ ->
    eventListener
      domcontentloaded
      (toEventTarget win)
      (\e -> Just $ ShareTargetEvent e)

{-------------------------------------------------------------------------------
| Make a reverse geolocation lookup of the current position.
-}
getPosition :: forall output m. MonadAff m => H.HalogenM State Action () output m Unit
getPosition = do
  poE <- H.liftAff $ getCurrentPosition $ unsafePartial $ fromJust $ setTimeout (Milliseconds 10000.0) defaultGeoLocOptions
  case poE of
    Left err -> H.liftEffect $ log $ show err
    Right pos -> do
      posString <- H.liftAff $ reverseGeoLocation bigDataGeolocURL bigDataGeolocResponse pos
      case posString of
        Left err -> H.liftEffect $ log $ "Reverse geolocation response failed to decode: " <> err
        Right dat -> do
          newStateAndSave newNoteStateLongDesc dat
          H.liftEffect $ log $ "Reverse geolocation response: " <> dat

{-------------------------------------------------------------------------------
| Helper function to load the `Note` from the local storage and put it into the
| `State`, the app's state.
-}
loadNote ::
  forall output m.
  MonadAff m =>
  Window ->
  H.HalogenM State Action () output m Unit
loadNote = loadObject newNoteState noteKeyId "Note"

{-------------------------------------------------------------------------------
| Helper function to load the `Options` from the local storage and put it into
| the `State`, the app's state.
-}
loadOptions ::
  forall output m.
  MonadAff m =>
  Window ->
  H.HalogenM State Action () output m Unit
loadOptions = loadObject newOptionsState optionsKeyId "Options"

{-------------------------------------------------------------------------------
| Helper function to deserialize an object from local storage and save it to the
| app's state of type `State`.
-}
loadObject ::
  forall a output m.
  StoreKey a =>
  DecodeJson a =>
  Show a =>
  MonadAff m =>
  (a -> H.HalogenM State Action () output m State) ->
  StoreKeyId -> String -> Window -> H.HalogenM State Action () output m Unit
loadObject storeToState keyId name win = do
  (loaded :: Maybe a) <- H.liftEffect $ loadFromLocalStorage win keyId
  case loaded of
    Nothing -> H.liftEffect $ log $ "No " <> name <> " loaded!"
    Just savedObj -> do
      currState <- storeToState savedObj
      H.liftEffect $ log $ "Loaded " <> name <> ": " <> show savedObj
      H.liftEffect $ log $ "New state is: " <> show currState

{-------------------------------------------------------------------------------
| Helper function: change the app's state using a function `f` with the new
| value `newVal` to set it (`f newVal` is called by newStateAndSave).
|
| The new state is saved to the local storage after setting the new state.
|
| * `f` - The function to use to set the new state.
| * `newVal` - The new value to set in the state.
-}
newStateAndSave ::
  forall output m a.
  MonadAff m =>
  (a -> H.HalogenM State Action () output m State) ->
  a ->
  H.HalogenM State Action () output m Unit
newStateAndSave f newVal = do
  newState <- f newVal
  win <- H.liftEffect $ window
  H.liftEffect $ saveToLocalStorage win newState.note
  H.liftEffect $ saveToLocalStorage win newState.options
