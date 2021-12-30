-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     OptionsSpec.purs
-- Date:     30.Dec.2021
--
-- ==============================================================================
-- | Module Test.Data.OptionsSpec, tests for the `Options` module.
module Test.Data.OptionsSpec
  ( spec
  ) where

import Prelude
import Test.Spec (Spec, describe, it)

{-------------------------------------------------------------------------------
| The tests to run.
|
| Automatically discovered by `spec-discover`, because of the name `spec`.
-}
spec :: Spec Unit
spec =
  describe "Data.Options - Tests" do
    describe "Group 1" do
      it "No Tests" $ pure unit
