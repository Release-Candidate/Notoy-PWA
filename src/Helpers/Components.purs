-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2022 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     Components.purs
-- Date:     04.Feb.2022
--
-- ==============================================================================
-- | Module Helpers.Components, functions to use with all Halogen components.
module Helpers.Components
  ( modifyComponentStateAndSave
  , modifyStateAndSave
  , setState
  ) where

import Prelude
import App.State (State)
import Data.Argonaut (class EncodeJson)
import Data.StoreKey (class StoreKey)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Helpers.Browser (saveToLocalStorage)
import Web.HTML (window)

{-------------------------------------------------------------------------------
| Helper function: change the app's state using a function `f` with the new
| value `newVal` to set it (`f newVal` is called by newStateAndSave).
|
| The new state is saved to the local storage after setting the new state.
|
| * `f` - The function to use to set the new state.
| * `newVal` - The new value to set in the state.
-}
modifyStateAndSave ::
  forall output m a action row.
  MonadAff m =>
  (a -> H.HalogenM State action (row) output m State) ->
  a ->
  H.HalogenM State action (row) output m Unit
modifyStateAndSave f newVal = do
  newStat <- f newVal
  win <- H.liftEffect $ window
  H.liftEffect $ saveToLocalStorage win newStat.note
  H.liftEffect $ saveToLocalStorage win newStat.options

{-------------------------------------------------------------------------------
| Change the state using a function `f` with the new value `newVal` to set it
| (`f newVal` is called by newStateAndSave).
|
| The new state is saved to the local storage after setting the new state.
|
| * `f` - The function to use to set the new state.
| * `newVal` - The new value to set in the state.
-}
modifyComponentStateAndSave ::
  forall m a action state output.
  MonadAff m =>
  StoreKey state =>
  EncodeJson state =>
  (a -> H.HalogenM state action () output m state) ->
  a ->
  H.HalogenM state action () output m Unit
modifyComponentStateAndSave f newVal = do
  newStat <- f newVal
  win <- H.liftEffect $ window
  H.liftEffect $ saveToLocalStorage win newStat

{-------------------------------------------------------------------------------
| Function to be used with `modifyStateAndSave` to save the given new state.
|
| Example:
|
| ```purescript
| modifyStateAndSave setState state
| ```
|
| * `state` - The new state to set.
-}
setState ∷
  ∀ action output state m.
  MonadAff m =>
  state →
  H.HalogenM state action () output m state
setState state = H.modify \_ -> state
