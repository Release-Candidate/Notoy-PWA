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
  , newNoteStateKeyWords
  , newNoteStateKeyWords_
  , newNoteStateLocation
  , newNoteStateLocation_
  , newNoteStateLongDesc
  , newNoteStateLongDesc_
  , newNoteStateShortDesc
  , newNoteStateShortDesc_
  , newNoteStateTitle
  , newNoteStateTitle_
  , newNoteStateUrl
  , newNoteStateUrl_
  , newNoteState_
  , newOptionsState
  , newOptionsStateAddDate
  , newOptionsStateAddDate_
  , newOptionsStateAddYamlHeader
  , newOptionsStateAddYamlHeader_
  , newOptionsStateFormat
  , newOptionsStateFormat_
  , newOptionsStateLookupLocation
  , newOptionsStateLookupLocation_
  , newOptionsState_
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType (MediaType(..))
import Data.Note (KeyWordArray, Note(..), defaultNote)
import Data.NoteContent (noteContentString)
import Data.Options (AddDate, AddYamlHeader, Format, LookupLocation, Options(..), defaultOptions, noteFileMime, noteFileSuffix)
import Data.URL (NoteURL)
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
  forall action output m.
  MonadAff m =>
  H.HalogenM State action () output m State
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
  forall action output m.
  MonadAff m =>
  Note -> H.HalogenM State action () output m State
newNoteState = newNoteStateGeneric H.modify

{-------------------------------------------------------------------------------
| Set the Note in the State to the given Note `note`.
|
| Does not return the new state.
|
| * `note` - The note to set in the new State.
-}
newNoteState_ ::
  forall action output m.
  MonadAff m =>
  Note -> H.HalogenM State action () output m Unit
newNoteState_ = newNoteStateGeneric H.modify_

{-------------------------------------------------------------------------------
| Set the title of the note in the state to `title`.
|
| Return the new state.
|
| * `title` - The new title of the note to set in the state.
-}
newNoteStateTitle ::
  forall action output m.
  MonadAff m =>
  String -> H.HalogenM State action () output m State
newNoteStateTitle = newNoteStateGenericTitle H.modify

{-------------------------------------------------------------------------------
| Set the title of the note in the state to `title`.
|
| Does not return the new state.
|
| * `title` - The new title of the note to set in the state.
-}
newNoteStateTitle_ ::
  forall action output m.
  MonadAff m =>
  String -> H.HalogenM State action () output m Unit
newNoteStateTitle_ = newNoteStateGenericTitle H.modify_

{-------------------------------------------------------------------------------
| Set the URL of the note in the state to `url`.
|
| Return the new state.
|
| * `url` - The new url of the note to set in the state.
-}
newNoteStateUrl ::
  forall action output m.
  MonadAff m =>
  Maybe NoteURL -> H.HalogenM State action () output m State
newNoteStateUrl = newNoteStateGenericUrl H.modify

{-------------------------------------------------------------------------------
| Set the URL of the note in the state to `url`.
|
| Does not return the new state.
|
| * `url` - The new url of the note to set in the state.
-}
newNoteStateUrl_ ::
  forall action output m.
  MonadAff m =>
  Maybe NoteURL -> H.HalogenM State action () output m Unit
newNoteStateUrl_ = newNoteStateGenericUrl H.modify_

{-------------------------------------------------------------------------------
| Set the keywords of the note in the state to `keywords`.
|
| Return the new state.
|
| * `keywords` - The new keywords of the note to set in the state.
-}
newNoteStateKeyWords ::
  forall action output m.
  MonadAff m =>
  Maybe KeyWordArray -> H.HalogenM State action () output m State
newNoteStateKeyWords = newNoteStateGenericKeyWords H.modify

{-------------------------------------------------------------------------------
| Set the keywords of the note in the state to `keywords`.
|
| Does not return the new state.
|
| * `keywords` - The new keywords of the note to set in the state.
-}
newNoteStateKeyWords_ ::
  forall action output m.
  MonadAff m =>
  Maybe KeyWordArray -> H.HalogenM State action () output m Unit
newNoteStateKeyWords_ = newNoteStateGenericKeyWords H.modify_

{-------------------------------------------------------------------------------
| Set the geolocation of the note in the state to `location`.
|
|  Return the new state.
|
| * `location` - The new geolocation of the note to set in the state.
-}
newNoteStateLocation ::
  forall action output m.
  MonadAff m =>
  String -> H.HalogenM State action () output m State
newNoteStateLocation = newNoteStateGenericLocation H.modify

{-------------------------------------------------------------------------------
| Set the geolocation of the note in the state to `location`.
|
|  Does not return the new state.
|
| * `location` - The new geolocation of the note to set in the state.
-}
newNoteStateLocation_ ::
  forall action output m.
  MonadAff m =>
  String -> H.HalogenM State action () output m Unit
newNoteStateLocation_ = newNoteStateGenericLocation H.modify_

{-------------------------------------------------------------------------------
| Set the short description of the note in the state to `shortDesc`.
|
|  Return the new state.
|
| * `shortDesc` - The new short description of the note to set in the state.
-}
newNoteStateShortDesc ::
  forall action output m.
  MonadAff m =>
  String -> H.HalogenM State action () output m State
newNoteStateShortDesc = newNoteStateGenericShort H.modify

{-------------------------------------------------------------------------------
| Set the short description of the note in the state to `shortDesc`.
|
|  Does not return the new state.
|
| * `shortDesc` - The new short description of the note to set in the state.
-}
newNoteStateShortDesc_ ::
  forall action output m.
  MonadAff m =>
  String -> H.HalogenM State action () output m Unit
newNoteStateShortDesc_ = newNoteStateGenericShort H.modify_

{-------------------------------------------------------------------------------
| Set the detailed description of the note in the state to `longDesc`.
|
|  Return the new state.
|
| * `longDesc` - The new detailed description of the note to set in the state.
-}
newNoteStateLongDesc ::
  forall action output m.
  MonadAff m =>
  String -> H.HalogenM State action () output m State
newNoteStateLongDesc = newNoteStateGenericLong H.modify

{-------------------------------------------------------------------------------
| Set the detailed description of the note in the state to `longDesc`.
|
|  Does not return the new state.
|
| * `longDesc` - The new detailed description of the note to set in the state.
-}
newNoteStateLongDesc_ ::
  forall action output m.
  MonadAff m =>
  String -> H.HalogenM State action () output m Unit
newNoteStateLongDesc_ = newNoteStateGenericLong H.modify_

{-------------------------------------------------------------------------------
| Set the options of the state to the given object.
|
| Return the new state.
|
| * `options` - The new Options to set in the state.
-}
newOptionsState ::
  forall action output m.
  MonadAff m =>
  Options -> H.HalogenM State action () output m State
newOptionsState = newOptionsStateGeneric H.modify

{-------------------------------------------------------------------------------
| Set the options of the state to the given object.
|
| Does not return the new state.
|
| * `options` - The new Options to set in the state.
-}
newOptionsState_ ::
  forall action output m.
  MonadAff m =>
  Options -> H.HalogenM State action () output m Unit
newOptionsState_ = newOptionsStateGeneric H.modify_

{-------------------------------------------------------------------------------
| Set the Format in the options of the state.
|
| Return the new state.
|
| * `format` - The new Format to set in the state.
-}
newOptionsStateFormat ::
  ∀ action output m.
  MonadAff m =>
  Format →
  H.HalogenM State action () output m State
newOptionsStateFormat = newOptionsStateGenericFormat H.modify

{-------------------------------------------------------------------------------
| Set the Format in the options of the state.
|
| Does not return the new state.
|
| * `format` - The new Format to set in the state.
-}
newOptionsStateFormat_ ::
  ∀ action output m.
  MonadAff m =>
  Format →
  H.HalogenM State action () output m Unit
newOptionsStateFormat_ = newOptionsStateGenericFormat H.modify_

{-------------------------------------------------------------------------------
| Set whether to add the current date to the note in the Options of the state.
|
| Return the new state.
|
| * `addDate` - The `AddDate` to set the Options in the state to.
-}
newOptionsStateAddDate ::
  ∀ action output m.
  MonadAff m =>
  AddDate →
  H.HalogenM State action () output m State
newOptionsStateAddDate = newOptionsStateGenericAddDate H.modify

{-------------------------------------------------------------------------------
| Set whether to add the current date to the note in the Options of the state.
|
| Does not return the new state.
|
| * `addDate` - The `AddDate` to set the Options in the state to.
-}
newOptionsStateAddDate_ ::
  ∀ action output m.
  MonadAff m =>
  AddDate →
  H.HalogenM State action () output m Unit
newOptionsStateAddDate_ = newOptionsStateGenericAddDate H.modify_

{-------------------------------------------------------------------------------
| Set whether to do a reverse geolocation lookup of the position in the Options
| of the state.
|
| Return the new state.
|
| * `lookupLocation` - The `LookupLocation` to set the Options in the state to.
-}
newOptionsStateLookupLocation ::
  ∀ action output m.
  MonadAff m =>
  LookupLocation →
  H.HalogenM State action () output m State
newOptionsStateLookupLocation = newOptionsStateGenericLookupLocation H.modify

{-------------------------------------------------------------------------------
| Set whether to do a reverse geolocation lookup of the position in the Options
| of the state.
|
| Does not return the new state.
|
| * `lookupLocation` - The `LookupLocation` to set the Options in the state to.
-}
newOptionsStateLookupLocation_ ::
  ∀ action output m.
  MonadAff m =>
  LookupLocation →
  H.HalogenM State action () output m Unit
newOptionsStateLookupLocation_ = newOptionsStateGenericLookupLocation H.modify_

{-------------------------------------------------------------------------------
| Set whether to add a YAML front matter header to the note in the Options of
| the state.
|
| Return the new state.
|
| * `addYamlHeader` - The `AddYamlHeader` to set the Options in the state to.
-}
newOptionsStateAddYamlHeader ::
  ∀ action output m.
  MonadAff m =>
  AddYamlHeader →
  H.HalogenM State action () output m State
newOptionsStateAddYamlHeader = newOptionsStateGenericAddYaml H.modify

{-------------------------------------------------------------------------------
| Set whether to add a YAML front matter header to the note in the Options of
| the state.
|
| Does not return the new state.
|
| * `addYamlHeader` - The `AddYamlHeader` to set the Options in the state to.
-}
newOptionsStateAddYamlHeader_ ::
  ∀ action output m.
  MonadAff m =>
  AddYamlHeader →
  H.HalogenM State action () output m Unit
newOptionsStateAddYamlHeader_ = newOptionsStateGenericAddYaml H.modify_

{-------------------------------------------------------------------------------
| Helper function: set the `Options` of the state.
-}
newOptionsStateGeneric ∷
  ∀ action output m a.
  MonadAff m =>
  ((State → State) → H.HalogenM State action () output m a) →
  Options →
  H.HalogenM State action () output m a
newOptionsStateGeneric f newOptions = f \state -> state { options = newOptions }

{-------------------------------------------------------------------------------
| Helper function: set the `Format` of the `Options` in the state.
-}
newOptionsStateGenericFormat ::
  ∀ action output m a.
  MonadAff m =>
  ((State → State) → H.HalogenM State action () output m a) →
  Format →
  H.HalogenM State action () output m a
newOptionsStateGenericFormat f newFormat =
  f \state ->
    let
      Options opts = state.options
    in
      state { options = Options opts { format = newFormat } }

{-------------------------------------------------------------------------------
| Helper function: set the `AddDate` of the `Options` in the state.
-}
newOptionsStateGenericAddDate ::
  ∀ action output m a.
  MonadAff m =>
  ((State → State) → H.HalogenM State action () output m a) →
  AddDate →
  H.HalogenM State action () output m a
newOptionsStateGenericAddDate f newAddDate =
  f \state ->
    let
      Options opts = state.options
    in
      state { options = Options opts { addDate = newAddDate } }

{-------------------------------------------------------------------------------
| Helper function: set the `LookupLocation` of the `Options` in the state.
-}
newOptionsStateGenericLookupLocation ::
  ∀ action output m a.
  MonadAff m =>
  ((State → State) → H.HalogenM State action () output m a) →
  LookupLocation →
  H.HalogenM State action () output m a
newOptionsStateGenericLookupLocation f newLookup =
  f \state ->
    let
      Options opts = state.options
    in
      state { options = Options opts { lookupLocation = newLookup } }

{-------------------------------------------------------------------------------
| Helper function: set the `AddYamlHeader` of the `Options` in the state.
-}
newOptionsStateGenericAddYaml ::
  ∀ action output m a.
  MonadAff m =>
  ((State → State) → H.HalogenM State action () output m a) →
  AddYamlHeader →
  H.HalogenM State action () output m a
newOptionsStateGenericAddYaml f newAddYaml =
  f \state ->
    let
      Options opts = state.options
    in
      state { options = Options opts { addYaml = newAddYaml } }

{-------------------------------------------------------------------------------
| Helper function: set the `Note` object of the state to a new one.
-}
newNoteStateGeneric ∷
  ∀ action output m a.
  MonadAff m =>
  ((State → State) → H.HalogenM State action () output m a) →
  Note →
  H.HalogenM State action () output m a
newNoteStateGeneric f newNote = f \state -> state { note = newNote }

{-------------------------------------------------------------------------------
| Helper function: set the title string of the `Note` in the state.
-}
newNoteStateGenericTitle ∷
  ∀ action output m a.
  MonadAff m =>
  ((State → State) → H.HalogenM State action () output m a) →
  String →
  H.HalogenM State action () output m a
newNoteStateGenericTitle f newTitle =
  f \state ->
    let
      Note n = state.note
    in
      state { note = Note n { title = Just newTitle } }

{-------------------------------------------------------------------------------
| Helper function: set the URL of the `Note` in the state.
-}
newNoteStateGenericUrl ∷
  ∀ action output m a.
  MonadAff m =>
  ((State → State) → H.HalogenM State action () output m a) →
  Maybe NoteURL →
  H.HalogenM State action () output m a
newNoteStateGenericUrl f newUrl =
  f \state ->
    let
      Note n = state.note
    in
      state { note = Note n { url = newUrl } }

{-------------------------------------------------------------------------------
| Helper function: set the keyword array of the `Note` in the state.
-}
newNoteStateGenericKeyWords ∷
  ∀ action output m a.
  MonadAff m =>
  ((State → State) → H.HalogenM State action () output m a) →
  Maybe KeyWordArray →
  H.HalogenM State action () output m a
newNoteStateGenericKeyWords f newKeywords =
  f \state ->
    let
      Note n = state.note
    in
      state { note = Note n { keywords = newKeywords } }

{-------------------------------------------------------------------------------
| Helper function: set the location string of the `Note` in the state.
-}
newNoteStateGenericLocation ∷
  ∀ action output m a.
  MonadAff m =>
  ((State → State) → H.HalogenM State action () output m a) →
  String →
  H.HalogenM State action () output m a
newNoteStateGenericLocation f newLocation =
  f \state ->
    let
      Note n = state.note
    in
      state { note = Note n { location = Just newLocation } }

{-------------------------------------------------------------------------------
| Helper function: set the short description string of the `Note` in the state.
-}
newNoteStateGenericShort ∷
  ∀ action output m a.
  MonadAff m =>
  ((State → State) → H.HalogenM State action () output m a) →
  String →
  H.HalogenM State action () output m a
newNoteStateGenericShort f newShortDesc =
  f \state ->
    let
      Note n = state.note
    in
      state { note = Note n { shortDesc = Just newShortDesc } }

{-------------------------------------------------------------------------------
| Helper function: set the detailed description string of the `Note` in the
| state.
-}
newNoteStateGenericLong ∷
  ∀ action output m a.
  MonadAff m =>
  ((State → State) → H.HalogenM State action () output m a) →
  String →
  H.HalogenM State action () output m a
newNoteStateGenericLong f newLongDesc =
  f \state ->
    let
      Note n = state.note
    in
      state { note = Note n { longDesc = Just newLongDesc } }
