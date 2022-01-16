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
  , getState
  , initialState
  , newNoteState
  , newNoteStateKeyWords
  , newNoteStateKeyWords_
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
  , newOptionsState_
  ) where

import Prelude
import Data.Argonaut ((.!=))
import Data.Maybe (Maybe(..))
import Data.Note (KeyWordArray, Note(..), defaultNote)
import Data.Options (AddDate, AddYamlHeader, Format, Options(..), defaultOptions)
import Data.URL (NoteURL)
import Effect.Aff.Class (class MonadAff)
import Halogen as H

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

newNoteStateShortDesc ::
  forall action output m.
  MonadAff m =>
  String -> H.HalogenM State action () output m State
newNoteStateShortDesc = newNoteStateGenericShort H.modify

newNoteStateShortDesc_ ::
  forall action output m.
  MonadAff m =>
  String -> H.HalogenM State action () output m Unit
newNoteStateShortDesc_ = newNoteStateGenericShort H.modify_

newNoteStateLongDesc ::
  forall action output m.
  MonadAff m =>
  String -> H.HalogenM State action () output m State
newNoteStateLongDesc = newNoteStateGenericLong H.modify

newNoteStateLongDesc_ ::
  forall action output m.
  MonadAff m =>
  String -> H.HalogenM State action () output m Unit
newNoteStateLongDesc_ = newNoteStateGenericLong H.modify_

newOptionsState ::
  forall action output m.
  MonadAff m =>
  Options -> H.HalogenM State action () output m State
newOptionsState = newOptionsStateGeneric H.modify

newOptionsState_ ::
  forall action output m.
  MonadAff m =>
  Options -> H.HalogenM State action () output m Unit
newOptionsState_ = newOptionsStateGeneric H.modify_

newOptionsStateFormat ::
  ∀ action output m.
  MonadAff m =>
  Format →
  H.HalogenM State action () output m State
newOptionsStateFormat = newOptionsStateGenericFormat H.modify

newOptionsStateFormat_ ::
  ∀ action output m.
  MonadAff m =>
  Format →
  H.HalogenM State action () output m Unit
newOptionsStateFormat_ = newOptionsStateGenericFormat H.modify_

newOptionsStateAddDate ::
  ∀ action output m.
  MonadAff m =>
  AddDate →
  H.HalogenM State action () output m State
newOptionsStateAddDate = newOptionsStateGenericAddDate H.modify

newOptionsStateAddDate_ ::
  ∀ action output m.
  MonadAff m =>
  AddDate →
  H.HalogenM State action () output m Unit
newOptionsStateAddDate_ = newOptionsStateGenericAddDate H.modify_

newOptionsStateAddYamlHeader ::
  ∀ action output m.
  MonadAff m =>
  AddYamlHeader →
  H.HalogenM State action () output m State
newOptionsStateAddYamlHeader = newOptionsStateGenericAddYaml H.modify

newOptionsStateAddYamlHeader_ ::
  ∀ action output m.
  MonadAff m =>
  AddYamlHeader →
  H.HalogenM State action () output m Unit
newOptionsStateAddYamlHeader_ = newOptionsStateGenericAddYaml H.modify_

newOptionsStateGeneric ∷
  ∀ action output m a.
  MonadAff m =>
  ((State → State) → H.HalogenM State action () output m a) →
  Options →
  H.HalogenM State action () output m a
newOptionsStateGeneric f newOptions =
  f \state ->
    { note: state.note
    , options: newOptions
    }

newOptionsStateGenericFormat ::
  ∀ action output m a.
  MonadAff m =>
  ((State → State) → H.HalogenM State action () output m a) →
  Format →
  H.HalogenM State action () output m a
newOptionsStateGenericFormat f newFormat =
  f \state ->
    let
      Options
        { addDate: oldAddDate
      , addYaml: oldAddYaml
      } = state.options
    in
      { note: state.note
      , options:
          Options
            { format: newFormat
            , addDate: oldAddDate
            , addYaml: oldAddYaml
            }
      }

newOptionsStateGenericAddDate ::
  ∀ action output m a.
  MonadAff m =>
  ((State → State) → H.HalogenM State action () output m a) →
  AddDate →
  H.HalogenM State action () output m a
newOptionsStateGenericAddDate f newAddDate =
  f \state ->
    let
      Options
        { format: oldFormat
      , addYaml: oldAddYaml
      } = state.options
    in
      { note: state.note
      , options:
          Options
            { format: oldFormat
            , addDate: newAddDate
            , addYaml: oldAddYaml
            }
      }

newOptionsStateGenericAddYaml ::
  ∀ action output m a.
  MonadAff m =>
  ((State → State) → H.HalogenM State action () output m a) →
  AddYamlHeader →
  H.HalogenM State action () output m a
newOptionsStateGenericAddYaml f newAddYaml =
  f \state ->
    let
      Options
        { format: oldFormat
      , addDate: oldAddDate
      } = state.options
    in
      { note: state.note
      , options:
          Options
            { format: oldFormat
            , addDate: oldAddDate
            , addYaml: newAddYaml
            }
      }

newNoteStateGeneric ∷
  ∀ action output m a.
  MonadAff m =>
  ((State → State) → H.HalogenM State action () output m a) →
  Note →
  H.HalogenM State action () output m a
newNoteStateGeneric f newNote =
  f \state ->
    { note: newNote
    , options: state.options
    }

newNoteStateGenericTitle ∷
  ∀ action output m a.
  MonadAff m =>
  ((State → State) → H.HalogenM State action () output m a) →
  String →
  H.HalogenM State action () output m a
newNoteStateGenericTitle f newTitle =
  f \state ->
    let
      Note
        { url: oldUrl
      , keywords: oldKeywords
      , shortDesc: oldShortDesc
      , longDesc: oldLongDesc
      } = state.note
    in
      { note:
          Note
            { title:
                Just newTitle
            , url: oldUrl
            , keywords: oldKeywords
            , shortDesc: oldShortDesc
            , longDesc: oldLongDesc
            }
      , options: state.options
      }

newNoteStateGenericUrl ∷
  ∀ action output m a.
  MonadAff m =>
  ((State → State) → H.HalogenM State action () output m a) →
  Maybe NoteURL →
  H.HalogenM State action () output m a
newNoteStateGenericUrl f newUrl =
  f \state ->
    let
      Note
        { title: oldTitle
      , keywords: oldKeywords
      , shortDesc: oldShortDesc
      , longDesc: oldLongDesc
      } = state.note
    in
      { note:
          Note
            { title: oldTitle
            , url: newUrl
            , keywords: oldKeywords
            , shortDesc: oldShortDesc
            , longDesc: oldLongDesc
            }
      , options: state.options
      }

newNoteStateGenericKeyWords ∷
  ∀ action output m a.
  MonadAff m =>
  ((State → State) → H.HalogenM State action () output m a) →
  Maybe KeyWordArray →
  H.HalogenM State action () output m a
newNoteStateGenericKeyWords f newKeywords =
  f \state ->
    let
      Note
        { title: oldTitle
      , url: oldUrl
      , shortDesc: oldShortDesc
      , longDesc: oldLongDesc
      } = state.note
    in
      { note:
          Note
            { title: oldTitle
            , url: oldUrl
            , keywords: newKeywords
            , shortDesc: oldShortDesc
            , longDesc: oldLongDesc
            }
      , options: state.options
      }

newNoteStateGenericShort ∷
  ∀ action output m a.
  MonadAff m =>
  ((State → State) → H.HalogenM State action () output m a) →
  String →
  H.HalogenM State action () output m a
newNoteStateGenericShort f newShortDesc =
  f \state ->
    let
      Note
        { title: oldTitle
      , url: oldUrl
      , keywords: oldKeywords
      , longDesc: oldLongDesc
      } = state.note
    in
      { note:
          Note
            { title: oldTitle
            , url: oldUrl
            , keywords: oldKeywords
            , shortDesc: Just newShortDesc
            , longDesc: oldLongDesc
            }
      , options: state.options
      }

newNoteStateGenericLong ∷
  ∀ action output m a.
  MonadAff m =>
  ((State → State) → H.HalogenM State action () output m a) →
  String →
  H.HalogenM State action () output m a
newNoteStateGenericLong f newLongDesc =
  f \state ->
    let
      Note
        { title: oldTitle
      , url: oldUrl
      , keywords: oldKeywords
      , shortDesc: oldShortDesc
      } = state.note
    in
      { note:
          Note
            { title: oldTitle
            , url: oldUrl
            , keywords: oldKeywords
            , shortDesc: oldShortDesc
            , longDesc: Just newLongDesc
            }
      , options: state.options
      }
