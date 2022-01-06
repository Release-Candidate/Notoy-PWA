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
import Data.Argonaut (decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Options (Options)
import Helpers.General (decodeJsonFromString, encodeToJsonString)
import Test.QuickCheck ((===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)

{-------------------------------------------------------------------------------
| The tests to run.
|
| Automatically discovered by `spec-discover`, because of the name `spec`.
-}
spec :: Spec Unit
spec =
  describe "Data.Options - Tests" do
    encodeDecodeSpecs

{-------------------------------------------------------------------------------
| Tests of the JSON de- and encoding.
-}
encodeDecodeSpecs :: Spec Unit
encodeDecodeSpecs =
  describe "Encode and decode to JSON" do
    it "Quickcheck decodeJson ° encodeJson"
      $ quickCheck \(option :: Options) ->
          (decodeJson $ encodeJson option) === Right option
    it "Quickcheck decodeJsonFromString ° encodeToJsonString"
      $ quickCheck \(option :: Options) ->
          (decodeJsonFromString $ encodeToJsonString option) === Right option
