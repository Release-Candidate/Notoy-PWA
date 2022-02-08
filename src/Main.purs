-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     Main.purs
-- Date:     23.Dec.2021
--
-- =============================================================================
-- | # Module Main
-- |
-- | Main entry point of the app.
module Main
  ( main
  ) where

import Prelude
import App.Action (Action(..), handleAction, Slots)
import App.Constants (appElementId, hiddenURLId)
import App.NoteComponent as N
import App.OptionsComponent as O
import App.State (State, initialState)
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Partial.Unsafe (unsafePartial)
import Web.DOM.ParentNode (QuerySelector(..))

{-------------------------------------------------------------------------------
|  Main entry point of the app.
-}
main :: Effect Unit
main =
  HA.runHalogenAff do
    HA.awaitLoad
    appEl <- HA.selectElement (QuerySelector $ "#" <> appElementId)
    let
      app = unsafePartial $ fromJust appEl
    runUI parentComponent unit app

{-------------------------------------------------------------------------------
| The main Halogen component, the parent of all other Halogen components.
-}
parentComponent ::
  forall query input output m. MonadAff m => H.Component query input output m
parentComponent =
  H.mkComponent
    { initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              }
    }

{-------------------------------------------------------------------------------
| Halogen's render function.
-}
render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div
    [ HP.id "all"
    , HP.classes
        [ ClassName "flex"
        , ClassName "flex-wrap"
        , ClassName "justify-center"
        ]
    ]
    [ HH.div [ HP.id "hiddenDiv" ]
        [ HH.a [ HP.id hiddenURLId, hiddenP true ]
            [ HH.text "download.md" ]
        ]
    , HH.slot N._noteComponent unit N.component state.note NoteAction
    , HH.slot_ O._optionsComponent unit O.component state.options
    ]

{-------------------------------------------------------------------------------
| HTML attribute `hidden`, to hide elements.
-}
hiddenP :: forall r i. Boolean -> HH.IProp ( hidden :: Boolean | r ) i
hiddenP = HH.prop (HH.PropName "hidden")
