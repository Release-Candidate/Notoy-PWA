-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     NoteSpec.purs
-- Date:     30.Dec.2021
--
-- ==============================================================================
-- | Module Test.Data.NoteSpec, the tests for the `Note` module.
module Test.Data.NoteSpec
  ( spec
  ) where

import Prelude
import Data.Foldable (for_)
import Data.Interpolate (interp)
import Data.Maybe (Maybe(..))
import Data.Note (Note(..), fromShared)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Web.URL (fromAbsolute)

{-------------------------------------------------------------------------------
| The tests to run.
|
| Automatically discovered by `spec-discover`, because of the name `spec`.
-}
spec :: Spec Unit
spec =
  describe "Data.Note - Tests" do
    fromSharedSpecs

fromSharedSpecs :: Spec Unit
fromSharedSpecs =
  describe "fromShared" do
    for_ fromSharedNotes \note@( Note
        { title: title
      , url: url
      , shortDesc: shortDesc
      , longDesc: _
      }
    ) ->
      it (interp "From Note: " (show note)) do
        fromShared title url shortDesc
          `shouldEqual`
            note

fromSharedNotes :: Array Note
fromSharedNotes =
  [ Note
      { title: Just "Title 1"
      , url: fromAbsolute "https://url.com:12354/index.html"
      , shortDesc: Just "Short text"
      , longDesc: Nothing
      }
  , Note
      { title: Just "Title 2"
      , url: Nothing
      , shortDesc: Just "Short text 2"
      , longDesc: Nothing
      }
  , Note
      { title: Just "Title 3"
      , url: Nothing
      , shortDesc: Nothing
      , longDesc: Nothing
      }
  , Note
      { title: Nothing
      , url: Nothing
      , shortDesc: Just "Short text 4"
      , longDesc: Nothing
      }
  , Note
      { title: Nothing
      , url: Nothing
      , shortDesc: Nothing
      , longDesc: Nothing
      }
  ]
