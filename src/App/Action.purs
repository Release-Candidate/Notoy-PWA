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
  , Slots
  , handleAction
  ) where

import Prelude
import App.BigDataGeoLoc (bigDataGeolocResponse, bigDataGeolocURL)
import App.Constants (hiddenURLId)
import App.Geolocation
  ( GeolocationPosition
  , defaultGeoLocOptions
  , getCurrentPosition
  , setTimeout
  , showLatitudeLongitude
  )
import App.NoteComponent as N
import App.OptionsComponent as O
import App.ShareTarget (handleShare, shareNote)
import App.State
  ( State
  , getState
  , setNoteState
  , setNoteState_
  , setOptionsState
  , setOptionsState_
  )
import Data.Argonaut (class DecodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Note (Note(..), noteKeyId)
import Data.Options (LookupLocation(..), Options(..), optionsKeyId)
import Data.StoreKey (class StoreKey, StoreKeyId)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.Query.Event (eventListener)
import Helpers.Browser
  ( downloadNote
  , loadFromLocalStorage
  , reverseGeoLocation
  )
import Helpers.Components (modifyStateAndSave)
import Partial.Unsafe (unsafePartial)
import Web.Event.Internal.Types (Event)
import Web.HTML (Window, window)
import Web.HTML.Event.EventTypes (domcontentloaded)
import Web.HTML.Window (toEventTarget)

{-------------------------------------------------------------------------------
| The Halogen `Slot`s of the child components.
-}
type Slots
  = ( note :: N.Slot
    , options :: O.Slot
    )

{-------------------------------------------------------------------------------
| The app's actions, emitted if an event has occurred.
|
| * Initialize - On initialization of the app.
| * ShareTargetEvent - If an URL has been shared with this app.
| * NoteAction - The output of the `NoteComponent`.
-}
data Action
  = Initialize
  | ShareTargetEvent Event
  | NoteAction N.Output

{-------------------------------------------------------------------------------
| The app's main `Action` (event) handler.
|
| Handles the app's events, by dispatching on event type `action`.
|
| * `action` - The `Action` to process.
-}
handleAction ::
  forall output m.
  MonadAff m =>
  Action -> H.HalogenM State Action Slots output m Unit
handleAction action = case action of
  Initialize -> appInit
  ShareTargetEvent e -> do
    win <- H.liftEffect window
    handleShare win e
  NoteAction output -> case output of
    N.Geolocation note -> do
      updateOptions
      setNoteState_ note
      getPosition
    N.Share note -> do
      H.liftAff $ shareNote note
    N.Download note -> do
      updateOptions
      state <- setNoteState note
      H.liftEffect $ downloadNote hiddenURLId state

{-------------------------------------------------------------------------------
| Initialization.
|
| Called on initialization of the app.
-}
appInit :: forall output m. MonadAff m => H.HalogenM State Action Slots output m Unit
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
|
| If `options.lookupLocation` is `NoReverseGeolocation`, do no reverse lookup,
| use the GPS coordinates.
-}
getPosition :: forall output m. MonadAff m => H.HalogenM State Action Slots output m Unit
getPosition = do
  poE <- H.liftAff $ getCurrPosTimeout (Milliseconds 10000.0)
  case poE of
    Left err -> H.liftEffect $ log $ show err
    Right pos -> do
      state <- getState
      let
        (Options options) = state.options
      case options.lookupLocation of
        NoReverseGeolocation -> savePosToState $ showLatitudeLongitude pos
        ReverseGeolocation -> do
          posString <- H.liftAff $ reverseGeoLocation bigDataGeolocURL bigDataGeolocResponse pos
          case posString of
            Left err -> do
              savePosToState $ showLatitudeLongitude pos
              H.liftEffect $ log $ "Reverse geolocation response failed to decode: " <> err
            Right dat -> do
              savePosToState dat
              H.liftEffect $ log $ "Reverse geolocation response: " <> dat

{-------------------------------------------------------------------------------
| Helper function: get current position with given timeout and default options.
|
| WARNING: unsafe, the `timeout` must be a valid timeout value!
-}
getCurrPosTimeout :: Milliseconds -> Aff (Either Error GeolocationPosition)
getCurrPosTimeout timeout =
  getCurrentPosition
    $ unsafePartial
    $ fromJust
    $ setTimeout timeout defaultGeoLocOptions

{-------------------------------------------------------------------------------
| Helper function to request the `Options` state from the child Component and
| save it to the state.
-}
updateOptions ::
  forall output m.
  MonadAff m =>
  H.HalogenM State Action Slots output m Unit
updateOptions = do
  options <- H.request O._optionsComponent unit O.GetOptions
  case options of
    Just opts -> setOptionsState_ opts
    Nothing -> pure unit

{-------------------------------------------------------------------------------
| Helper function to save the position to the state.
-}
savePosToState ::
  forall output m.
  MonadAff m =>
  String ->
  H.HalogenM State Action Slots output m Unit
savePosToState = modifyStateAndSave setNoteStateLocation

{-------------------------------------------------------------------------------
| Helper function to load the `Note` from the local storage and put it into the
| `State`, the app's state.
-}
loadNote ::
  forall output m.
  MonadAff m =>
  Window ->
  H.HalogenM State Action Slots output m Unit
loadNote = loadObject setNoteState noteKeyId "Note"

{-------------------------------------------------------------------------------
| Helper function to load the `Options` from the local storage and put it into
| the `State`, the app's state.
-}
loadOptions ::
  forall output m.
  MonadAff m =>
  Window ->
  H.HalogenM State Action Slots output m Unit
loadOptions = loadObject setOptionsState optionsKeyId "Options"

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
  (a -> H.HalogenM State Action Slots output m State) ->
  StoreKeyId -> String -> Window -> H.HalogenM State Action Slots output m Unit
loadObject storeToState keyId name win = do
  (loaded :: Maybe a) <- H.liftEffect $ loadFromLocalStorage win keyId
  case loaded of
    Nothing -> H.liftEffect $ log $ "No " <> name <> " loaded!"
    Just savedObj -> do
      currState <- storeToState savedObj
      H.liftEffect $ log $ "Loaded " <> name <> ": " <> show savedObj
      H.liftEffect $ log $ "New state is: " <> show currState

{-------------------------------------------------------------------------------
| Helper function: set the location string of the `Note` in the state.
-}
setNoteStateLocation ∷
  ∀ output m.
  MonadAff m =>
  String →
  H.HalogenM State Action Slots output m State
setNoteStateLocation newLocation =
  H.modify \state ->
    let
      Note n = state.note
    in
      state { note = Note n { location = Just newLocation } }
