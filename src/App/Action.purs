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
import App.ShareTarget (handleShare)
import App.State
  ( State
  , newNoteState
  , newNoteStateKeyWords
  , newNoteStateLongDesc
  , newNoteStateShortDesc
  , newNoteStateTitle
  , newNoteStateUrl
  , newOptionsStateAddDate
  , newOptionsStateAddYamlHeader
  , newOptionsStateFormat
  )
import Data.Maybe (Maybe(..))
import Data.Note (Note, keyWordArrayFromString, noteKeyId)
import Data.Options (addDateFromBool, formatFromString, yamlHeaderFromBool)
import Data.URL (noteUrlFromString)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.Query.Event (eventListener)
import Helpers.Browser (loadFromLocalStorage, saveToLocalStorage)
import Web.Event.Internal.Types (Event)
import Web.HTML (window)
import Web.HTML.Event.EventTypes (domcontentloaded)
import Web.HTML.Window (toEventTarget)

{-------------------------------------------------------------------------------
| The app's actions, called if an event occurred.
-}
data Action
  = Initialize
  | ShareTargetEvent Event
  | TitleChanged String
  | URLChanged String
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
    win <- H.liftEffect $ window
    handleShare win e
  TitleChanged st -> newStateAndSave newNoteStateTitle st
  URLChanged st -> newStateAndSave (newNoteStateUrl <<< noteUrlFromString) st
  KeywordsChanged st -> newStateAndSave (newNoteStateKeyWords <<< keyWordArrayFromString) st
  ShortDescChanged st -> newStateAndSave newNoteStateShortDesc st
  LongDescChanged st -> newStateAndSave newNoteStateLongDesc st
  FormatChanged st -> newStateAndSave (newOptionsStateFormat <<< formatFromString) st
  AddDateChanged b -> newStateAndSave (newOptionsStateAddDate <<< addDateFromBool) b
  AddYamlHeaderChanged b -> newStateAndSave (newOptionsStateAddYamlHeader <<< yamlHeaderFromBool) b
  ShareNote -> pure unit
  DownloadNote -> pure unit

{-------------------------------------------------------------------------------
| Initialization.
|
| called on initialization of the app.
-}
appInit :: forall output m. MonadAff m => H.HalogenM State Action () output m Unit
appInit = do
  win <- H.liftEffect $ window
  (loadedNote :: Maybe Note) <- H.liftEffect $ loadFromLocalStorage win noteKeyId
  case loadedNote of
    Nothing -> H.liftEffect $ log $ "No Note loaded!"
    Just savedNote -> do
      currState <- newNoteState savedNote
      H.liftEffect $ log $ "Loaded Note: " <> show currState.note
  H.subscribe' \_ ->
    eventListener
      domcontentloaded
      (toEventTarget win)
      (\e -> Just $ ShareTargetEvent e)

{-------------------------------------------------------------------------------
| Helper function: change the app's state using a function `f` with the new
| value `newVal` to set it.
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
  win <- H.liftEffect $ window
  newState <- f newVal
  H.liftEffect $ saveToLocalStorage win newState.note
  H.liftEffect $ saveToLocalStorage win newState.options
