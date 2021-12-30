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
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

{-------------------------------------------------------------------------------
| The tests to run.
|
| Automatically discovered by `spec-discover`, because of the name `spec`.
-}
spec :: Spec Unit
spec =
  describe "Data.Note - Tests" do
    describe "Group 1" do
      it "No Tests" $ pure unit
