-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     Main.purs
-- Date:     30.Dec.2021
--
-- ==============================================================================
-- | Module test.Main, the main Entry Point of the tests.
module Test.Main
  ( main
  ) where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

{-------------------------------------------------------------------------------
| Main entry point of the tests.
|
| Uses autodiscovery to run all tests.
-}
main :: Effect Unit
main =
  launchAff_ do
    tests <- discover "Test\\..*Spec"
    runSpec [ consoleReporter ] tests
