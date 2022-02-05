-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2022 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     Components.purs
-- Date:     04.Feb.2022
--
-- ==============================================================================
-- | Module App.Components, functions to use with all Halogen components.
module App.Components
  ( newState
  , newStateAndSave
  ) where

import Prelude
import Data.Argonaut (class EncodeJson)
import Data.StoreKey (class StoreKey)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Helpers.Browser (saveToLocalStorage)
import Web.HTML (window)

{-------------------------------------------------------------------------------
| Change the state using a function `f` with the new value `newVal` to set it
| (`f newVal` is called by newStateAndSave).
|
| The new state is saved to the local storage after setting the new state.
|
| * `f` - The function to use to set the new state.
| * `newVal` - The new value to set in the state.
-}
newStateAndSave ::
  forall m a action state output.
  MonadAff m =>
  StoreKey state =>
  EncodeJson state =>
  (a -> H.HalogenM state action () output m state) ->
  a ->
  H.HalogenM state action () output m Unit
newStateAndSave f newVal = do
  newStat <- f newVal
  win <- H.liftEffect $ window
  H.liftEffect $ saveToLocalStorage win newStat

{-------------------------------------------------------------------------------
| Function to be used with `newStateAndSave` to save the given new state.
|
| Example:
|
| ```purescript
| newStateAndSave newState note
| ```
|
| * `state` - The new state to set.
-}
newState ∷
  ∀ action output state m.
  MonadAff m =>
  state →
  H.HalogenM state action () output m state
newState state = H.modify \_ -> state
