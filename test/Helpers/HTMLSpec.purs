-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     HTMLSpec.purs
-- Date:     30.Dec.2021
--
-- ==============================================================================
-- | Module Helpers.HTMLSpec, tests for the module `Helpers.HTML`.
module Test.Helpers.HTMLSpec
  ( spec
  ) where

import Prelude
import Test.Spec (Spec, describe, it, parallel)

{-------------------------------------------------------------------------------
| The tests to run.
|
| Automatically discovered by `spec-discover`, because of the name `spec`.
-}
spec :: Spec Unit
spec =
  describe "Helpers.HTML - Tests"
    $ parallel do
        describe "Group 1" do
          it "Does not work without a browser!" $ pure unit
