-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2022 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     State.purs
-- Date:     16.Jan.2022
--
-- ==============================================================================
-- | Module App.State, the app's state.
module App.State
  ( State
  , filenameFromState
  , getState
  , initialState
  , makeBlob
  , newNoteState
  , newNoteState_
  , newOptionsState
  , newOptionsState_
  ) where

import Prelude
import Data.Maybe (fromMaybe)
import Data.MediaType (MediaType(..))
import Data.Note (Note(..), defaultNote)
import Data.NoteContent (noteContentString)
import Data.Options (Options, defaultOptions, noteFileMime, noteFileSuffix)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Helpers.General (sanitizeFileName)
import Web.File.Blob as Blob

{-------------------------------------------------------------------------------
| The App's state.
-}
type State
  = { options :: Options
    , note :: Note
    }

{-------------------------------------------------------------------------------
| The initial state of the app.
-}
initialState :: forall input. input -> State
initialState _ =
  { options: defaultOptions
  , note: defaultNote
  }

{-------------------------------------------------------------------------------
| Return the current state.
-}
getState ::
  forall action output m row state.
  MonadAff m =>
  H.HalogenM state action (row) output m state
getState = H.get

{-------------------------------------------------------------------------------
| Return the formatted note as a string.
|
| * `state` - The `State` holding the `Note` and `Options` needed.
-}
makeBlob :: State -> Blob.Blob
makeBlob state = Blob.fromString content mediaType
  where
  content = noteContentString state.options state.note

  mediaType = MediaType $ noteFileMime state.options

{-------------------------------------------------------------------------------
| Return a filename from a `State`.
|
| Use the title of the note as a filename, with the suffix of the `Format`.!=
|
| * `state` - The `State` holding the `Note` and `Options`.
-}
filenameFromState :: State -> String
filenameFromState state = sanitizeFileName title <> noteFileSuffix state.options
  where
  Note { title: titleM } = state.note

  title = fromMaybe "note" titleM

{-------------------------------------------------------------------------------
| Set the Note in the State to the given Note `note`.
|
| Return the new state.
|
| * `note` - The note to set in the new State.
-}
newNoteState ::
  forall action output m row.
  MonadAff m =>
  Note -> H.HalogenM State action (row) output m State
newNoteState = newNoteStateGeneric H.modify

{-------------------------------------------------------------------------------
| Set the Note in the State to the given Note `note`.
|
| Does not return the new state.
|
| * `note` - The note to set in the new State.
-}
newNoteState_ ::
  forall action output m row.
  MonadAff m =>
  Note -> H.HalogenM State action (row) output m Unit
newNoteState_ = newNoteStateGeneric H.modify_

{-------------------------------------------------------------------------------
| Set the options of the state to the given object.
|
| Return the new state.
|
| * `options` - The new Options to set in the state.
-}
newOptionsState ::
  forall action output m row.
  MonadAff m =>
  Options -> H.HalogenM State action (row) output m State
newOptionsState = newOptionsStateGeneric H.modify

{-------------------------------------------------------------------------------
| Set the options of the state to the given object.
|
| Does not return the new state.
|
| * `options` - The new Options to set in the state.
-}
newOptionsState_ ::
  forall action output m row.
  MonadAff m =>
  Options -> H.HalogenM State action (row) output m Unit
newOptionsState_ = newOptionsStateGeneric H.modify_

{-------------------------------------------------------------------------------
| Helper function: set the `Options` of the state.
-}
newOptionsStateGeneric ∷
  ∀ action output m a row.
  MonadAff m =>
  ((State → State) → H.HalogenM State action (row) output m a) →
  Options →
  H.HalogenM State action (row) output m a
newOptionsStateGeneric f newOptions = f \state -> state { options = newOptions }

{-------------------------------------------------------------------------------
| Helper function: set the `Note` object of the state to a new one.
-}
newNoteStateGeneric ∷
  ∀ action output m a row.
  MonadAff m =>
  ((State → State) → H.HalogenM State action (row) output m a) →
  Note →
  H.HalogenM State action (row) output m a
newNoteStateGeneric f newNote = f \state -> state { note = newNote }
