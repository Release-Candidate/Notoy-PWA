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
  , setNoteState
  , setNoteState_
  , setOptionsState
  , setOptionsState_
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
| * `timestamp` - The date to use if it should be added to the note.
| * `language` - The language this note is written in, for the YAML front
|                matter.
-}
makeBlob :: State -> String -> String -> Blob.Blob
makeBlob state timestamp language = Blob.fromString content mediaType
  where
  content = noteContentString state.options state.note timestamp language

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
setNoteState ::
  forall action output m row.
  MonadAff m =>
  Note -> H.HalogenM State action (row) output m State
setNoteState = setNoteStateGeneric H.modify

{-------------------------------------------------------------------------------
| Set the Note in the State to the given Note `note`.
|
| Does not return the new state.
|
| * `note` - The note to set in the new State.
-}
setNoteState_ ::
  forall action output m row.
  MonadAff m =>
  Note -> H.HalogenM State action (row) output m Unit
setNoteState_ = setNoteStateGeneric H.modify_

{-------------------------------------------------------------------------------
| Set the options of the state to the given object.
|
| Return the new state.
|
| * `options` - The new Options to set in the state.
-}
setOptionsState ::
  forall action output m row.
  MonadAff m =>
  Options -> H.HalogenM State action (row) output m State
setOptionsState = setOptionsStateGeneric H.modify

{-------------------------------------------------------------------------------
| Set the options of the state to the given object.
|
| Does not return the new state.
|
| * `options` - The new Options to set in the state.
-}
setOptionsState_ ::
  forall action output m row.
  MonadAff m =>
  Options -> H.HalogenM State action (row) output m Unit
setOptionsState_ = setOptionsStateGeneric H.modify_

{-------------------------------------------------------------------------------
| Helper function: set the `Options` of the state.
-}
setOptionsStateGeneric ∷
  ∀ action output m a row.
  MonadAff m =>
  ((State → State) → H.HalogenM State action (row) output m a) →
  Options →
  H.HalogenM State action (row) output m a
setOptionsStateGeneric f newOptions = f \state -> state { options = newOptions }

{-------------------------------------------------------------------------------
| Helper function: set the `Note` object of the state to a new one.
-}
setNoteStateGeneric ∷
  ∀ action output m a row.
  MonadAff m =>
  ((State → State) → H.HalogenM State action (row) output m a) →
  Note →
  H.HalogenM State action (row) output m a
setNoteStateGeneric f newNote = f \state -> state { note = newNote }
