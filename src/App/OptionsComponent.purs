-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2022 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     OptionsComponent.purs
-- Date:     03.Feb.2022
--
-- ==============================================================================
-- | Module App.OptionsComponent, the Halogen component for the options of the
-- | app.
module App.OptionsComponent
  ( Action(..)
  , Query(..)
  , Slot
  , _optionsComponent
  , component
  ) where

import Prelude
import App.State (getState)
import Data.Maybe (Maybe(..))
import Data.Options (AddDate(..), AddYamlHeader(..), Format(..), LookupLocation(..), Options(..), addDateFromBool, formatFromString, lookupLocationFromBool, yamlHeaderFromBool)
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Helpers.Components (modifyComponentStateAndSave, setState)
import Type.Proxy (Proxy(..))

{-------------------------------------------------------------------------------
| The proxy for an `OptionsComponent`, to use in the slot of the parent
| component.
-}
_optionsComponent = Proxy :: Proxy "options"

{-------------------------------------------------------------------------------
| The Halogen slot type of the options component.
|
| Only one instance is allowed, the id is `Unit`.
-}
type Slot
  = H.Slot Query Void Unit

{-------------------------------------------------------------------------------
| The type for queries from the parent component.
|
| * GetOptions - Send the current `Options` state to the parent component.
-}
data Query a
  = GetOptions (Options -> a)

{-------------------------------------------------------------------------------
| The actions (events) of the component.
|
| * Receive - Input from the parent component received.
| * FormatChanged - The format of a note has changed.
| * AddDateChanged - Whether to add the date to the note has changed.
| * ReverseGeolocChanged - Whether to do reverse geolocation has changed.
| * AddYamlHeaderChanged - Whether to add a YAML front matter header has changed.
-}
data Action
  = Receive Options
  | FormatChanged String
  | AddDateChanged Boolean
  | ReverseGeolocChanged Boolean
  | AddYamlHeaderChanged Boolean

{-------------------------------------------------------------------------------
| The `OptionsComponent` description for use in the parent component.
-}
component ∷ ∀ output m. MonadAff m => H.Component Query Options output m
component =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              , receive = Just <<< Receive
              }
    }
  where
  initialState options = options

{-------------------------------------------------------------------------------
| The component's `Action` (event) handler.
|
| Handles the component's events, by dispatching on event type `action`.
|
| * `action` - The `Action` to process.
-}
handleAction ::
  forall output m.
  MonadAff m =>
  Action -> H.HalogenM Options Action () output m Unit
handleAction action = case action of
  Receive options -> do
    currState <- getState
    case currState == options of
      true -> pure unit
      false -> modifyComponentStateAndSave setState options
  FormatChanged st -> modifyComponentStateAndSave (setOptionsStateFormat <<< formatFromString) st
  AddDateChanged b -> modifyComponentStateAndSave (setOptionsStateAddDate <<< addDateFromBool) b
  ReverseGeolocChanged b -> modifyComponentStateAndSave (setOptionsStateLookupLocation <<< lookupLocationFromBool) b
  AddYamlHeaderChanged b -> modifyComponentStateAndSave (setOptionsStateAddYamlHeader <<< yamlHeaderFromBool) b

{-------------------------------------------------------------------------------
| The handler for queries from the parent component.
|
| * `q` - The `Query` to process.
-}
handleQuery ::
  forall m a output.
  MonadAff m =>
  Query a -> H.HalogenM Options Action () output m (Maybe a)
handleQuery q = case q of
  GetOptions k -> do
    options <- getState
    pure $ Just $ k options

{-------------------------------------------------------------------------------
| The Halogen render function of this component.
-}
render :: forall m. MonadAff m => Options -> H.ComponentHTML Action () m
render opts =
  let
    Options options = opts
  in
    HH.div
      [ HP.id "options"
      , HP.classes
          [ ClassName "container"
          , ClassName "mx-auto"
          , ClassName "space-y-3"
          ]
      ]
      [ HH.div [ HP.id "format" ]
          [ HH.fieldset
              [ HP.id "formatParent"
              , HP.classes []
              ]
              [ HH.legend []
                  [ HH.text "Note file format" ]
              , HH.label [ HP.for "markdown", HP.classes [ ClassName "block" ] ]
                  [ HH.input
                      [ HP.id "markdown"
                      , HP.name "formatRadio"
                      , HP.type_ HP.InputRadio
                      , HP.value $ show Markdown
                      , HP.checked $ options.format == Markdown
                      , HE.onChecked \_ -> FormatChanged $ show Markdown
                      ]
                  , HH.text "Markdown (Obsidian, Joplin, Zettlr)"
                  ]
              , HH.label [ HP.for "orgMode", HP.classes [ ClassName "block" ] ]
                  [ HH.input
                      [ HP.id "orgMode"
                      , HP.name "formatRadio"
                      , HP.type_ HP.InputRadio
                      , HP.value $ show OrgMode
                      , HP.checked $ options.format == OrgMode
                      , HE.onChecked \_ -> FormatChanged $ show OrgMode
                      ]
                  , HH.text "Org-Mode (Emacs)"
                  ]
              , HH.label [ HP.for "text", HP.classes [ ClassName "block" ] ]
                  [ HH.input
                      [ HP.id "text"
                      , HP.name "formatRadio"
                      , HP.type_ HP.InputRadio
                      , HP.value $ show Text
                      , HP.checked $ options.format == Text
                      , HE.onChecked \_ -> FormatChanged $ show Text
                      ]
                  , HH.text "Plain Text"
                  ]
              ]
          ]
      , HH.div [ HP.id "timestamp" ]
          [ HH.label [ HP.for "timestampInput", HP.classes [ ClassName "block" ] ]
              [ HH.span_ [ HH.text "Add the current date to the note?" ]
              , HH.input
                  [ HP.type_ HP.InputCheckbox
                  , HP.id "timestampInput"
                  , HP.checked $ options.addDate == AddDate
                  , HE.onChecked \b -> AddDateChanged b
                  ]
              ]
          ]
      , HH.div [ HP.id "reverseGeolocation" ]
          [ HH.label [ HP.for "reverseGeolocationInput", HP.classes [ ClassName "block" ] ]
              [ HH.span_ [ HH.text "Look the position up on BigData?" ]
              , HH.input
                  [ HP.type_ HP.InputCheckbox
                  , HP.id "reverseGeolocationInput"
                  , HP.checked $ options.lookupLocation == ReverseGeolocation
                  , HE.onChecked \b -> ReverseGeolocChanged b
                  ]
              ]
          ]
      , HH.div [ HP.id "yaml" ]
          [ HH.label [ HP.for "yamlFrontMatter", HP.classes [ ClassName "block" ] ]
              [ HH.span_ [ HH.text "Add YAML front matter (YAML metadata block for Pandoc)?" ]
              , HH.input
                  [ HP.type_ HP.InputCheckbox
                  , HP.id "yamlFrontMatter"
                  , HP.checked $ options.addYaml == AddYamlHeader
                  , HE.onChecked \b -> AddYamlHeaderChanged b
                  ]
              ]
          ]
      ]

{-------------------------------------------------------------------------------
| Helper function: set the `Format` of the `Options` in the state.
|
| For use with `modifyComponentStateAndSave`.
-}
setOptionsStateFormat ::
  ∀ action output m.
  MonadAff m =>
  Format →
  H.HalogenM Options action () output m Options
setOptionsStateFormat newFormat = H.modify \(Options options) -> Options options { format = newFormat }

{-------------------------------------------------------------------------------
| Helper function: set the `AddDate` of the `Options` in the state.
|
| For use with `modifyComponentStateAndSave`.
-}
setOptionsStateAddDate ::
  ∀ action output m.
  MonadAff m =>
  AddDate →
  H.HalogenM Options action () output m Options
setOptionsStateAddDate newAddDate = H.modify \(Options options) -> Options options { addDate = newAddDate }

{-------------------------------------------------------------------------------
| Helper function: set the `LookupLocation` of the `Options` in the state.
|
| For use with `modifyComponentStateAndSave`.
-}
setOptionsStateLookupLocation ::
  ∀ action output m.
  MonadAff m =>
  LookupLocation →
  H.HalogenM Options action () output m Options
setOptionsStateLookupLocation newLookup = H.modify \(Options options) -> Options options { lookupLocation = newLookup }

{-------------------------------------------------------------------------------
| Helper function: set the `AddYamlHeader` of the `Options` in the state.
|
| For use with `modifyComponentStateAndSave`.
-}
setOptionsStateAddYamlHeader ::
  ∀ action output m.
  MonadAff m =>
  AddYamlHeader →
  H.HalogenM Options action () output m Options
setOptionsStateAddYamlHeader newAddYaml = H.modify \(Options options) -> Options options { addYaml = newAddYaml }
