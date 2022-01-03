-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2022 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     BrowserSpec.purs
-- Date:     30.Dec.2021
--
-- ==============================================================================
-- | Module Test.Helpers.BrowserSpec, tests for the module `Helpers.HTML`.
module Test.Helpers.BrowserSpec
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
  describe "Helpers.Browser - Tests"
    $ parallel do
        describe "Group 1" do
          it "Does not work without a browser!" $ pure unit
