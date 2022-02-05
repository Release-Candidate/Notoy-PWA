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
  , optionsComponent
  ) where

import Prelude
import App.Components (newState, newStateAndSave)
import App.State (getState)
import Data.Maybe (Maybe(..))
import Data.Options
  ( AddDate(..)
  , AddYamlHeader(..)
  , Format(..)
  , LookupLocation(..)
  , Options(..)
  , addDateFromBool
  , formatFromString
  , lookupLocationFromBool
  , yamlHeaderFromBool
  )
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

_optionsComponent = Proxy :: Proxy "options"

{-------------------------------------------------------------------------------
| The Halogen slot type of the options component.
|
| Only one instance is allowed, the id is `Unit`.
-}
type Slot
  = H.Slot Query Void Unit

data Query a
  = GetOptions (Options -> a)

data Action
  = Receive Options
  | FormatChanged String
  | AddDateChanged Boolean
  | ReverseGeolocChanged Boolean
  | AddYamlHeaderChanged Boolean

optionsComponent ∷ ∀ output m. MonadAff m => H.Component Query Options output m
optionsComponent =
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
| The app's main `Action` (event) handler.
|
| Handles the app's events, by dispatching on event type `e`.
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
      false -> newStateAndSave newState options
  FormatChanged st -> newStateAndSave (newOptionsStateFormat <<< formatFromString) st
  AddDateChanged b -> newStateAndSave (newOptionsStateAddDate <<< addDateFromBool) b
  ReverseGeolocChanged b -> newStateAndSave (newOptionsStateLookupLocation <<< lookupLocationFromBool) b
  AddYamlHeaderChanged b -> newStateAndSave (newOptionsStateAddYamlHeader <<< yamlHeaderFromBool) b

handleQuery ::
  forall m a output.
  MonadAff m =>
  Query a -> H.HalogenM Options Action () output m (Maybe a)
handleQuery q = case q of
  GetOptions k -> do
    options <- getState
    pure $ Just $ k options

render :: forall m. MonadAff m => Options -> H.ComponentHTML Action () m
render opts =
  let
    Options options = opts
  in
    HH.div [ HP.id "options" ]
      [ HH.div [ HP.id "format" ]
          [ HH.fieldset
              [ HP.id "formatParent" ]
              [ HH.legend [] [ HH.text "Note file format" ]
              , HH.label [ HP.for "markdown" ]
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
              , HH.label [ HP.for "orgMode" ]
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
              , HH.label [ HP.for "text" ]
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
          [ HH.label [ HP.for "timestampInput" ]
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
          [ HH.label [ HP.for "reverseGeolocationInput" ]
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
          [ HH.label [ HP.for "yamlFrontMatter" ]
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
-}
newOptionsStateFormat ::
  ∀ action output m.
  MonadAff m =>
  Format →
  H.HalogenM Options action () output m Options
newOptionsStateFormat newFormat = H.modify \(Options options) -> Options options { format = newFormat }

{-------------------------------------------------------------------------------
| Helper function: set the `AddDate` of the `Options` in the state.
-}
newOptionsStateAddDate ::
  ∀ action output m.
  MonadAff m =>
  AddDate →
  H.HalogenM Options action () output m Options
newOptionsStateAddDate newAddDate = H.modify \(Options options) -> Options options { addDate = newAddDate }

{-------------------------------------------------------------------------------
| Helper function: set the `LookupLocation` of the `Options` in the state.
-}
newOptionsStateLookupLocation ::
  ∀ action output m.
  MonadAff m =>
  LookupLocation →
  H.HalogenM Options action () output m Options
newOptionsStateLookupLocation newLookup = H.modify \(Options options) -> Options options { lookupLocation = newLookup }

{-------------------------------------------------------------------------------
| Helper function: set the `AddYamlHeader` of the `Options` in the state.
-}
newOptionsStateAddYamlHeader ::
  ∀ action output m.
  MonadAff m =>
  AddYamlHeader →
  H.HalogenM Options action () output m Options
newOptionsStateAddYamlHeader newAddYaml = H.modify \(Options options) -> Options options { addYaml = newAddYaml }
