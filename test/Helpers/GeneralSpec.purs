-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     GeneralSpec.purs
-- Date:     30.Dec.2021
--
-- ==============================================================================
-- | Module Helpers.GeneralSpec, tests for the `Helpers.General` module.
module Test.Helpers.GeneralSpec
  ( spec
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import Helpers.General (getFirstMatch, getURL)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

{-------------------------------------------------------------------------------
| The tests to run.
|
| Automatically discovered by `spec-discover`, because of the name `spec`.
-}
spec :: Spec Unit
spec =
  describe "Helpers.General - Tests" do
    getFirstMatchSpecs
    getURLSpecs

{-------------------------------------------------------------------------------
| Tests for the function `getFirstMatch`.
-}
getFirstMatchSpecs :: Spec Unit
getFirstMatchSpecs =
  describe "getFirstMatch" do
    it "testRegex1: No match -> Nothing" do
      getFirstMatch testRegex1 "Hugo" `shouldEqual` Nothing
    it "testRegex1: Match AFooda -> Foo" do
      getFirstMatch testRegex1 "AFooda" `shouldEqual` Just "Foo"
    it "testRegex1: Match AFoodFooa -> Foo" do
      getFirstMatch testRegex1 "AFoodFooa" `shouldEqual` Just "Foo"
    it "testRegex2: No match -> Nothing" do
      getFirstMatch testRegex2 "Hugo" `shouldEqual` Nothing
    it "testRegex2: Match AFooda -> Foo" do
      getFirstMatch testRegex2 "A Foo da" `shouldEqual` Just "Foo"
    it "testRegex2: Match Unicode -> FoodFoo习近平讲党史故事" do
      getFirstMatch testRegex2 "重温习主席这些新年贺词，我们豪情万丈FoodFoo习近平讲党史故事 出版发行 拼出我要的未来" `shouldEqual` Just "FoodFoo习近平讲党史故事"

testRegex1 ∷ Regex
testRegex1 = unsafeRegex "Foo" unicode

testRegex2 ∷ Regex
testRegex2 = unsafeRegex "F\\p{L}+" unicode

{-------------------------------------------------------------------------------
| Tests for the function `getURL`.
-}
getURLSpecs :: Spec Unit
getURLSpecs =
  describe "getURL" do
    it "No Url -> Nothing" do
      getURL "This is not an URL!" `shouldEqual` Nothing
    it "URL 1 https://pursuit.purescript.org/search?q=spec+dsf+hfgh++gfh -> URL" do
      let
        u = "https://pursuit.purescript.org/search?q=spec+dsf+hfgh++gfh"
      getURL u `shouldEqual` Just u
    it "URL 2 https://pursuit.purescript.org/search?q=spec+dsf+hfgh++gfh -> URL" do
      let
        u = "https://pursuit.purescript.org/search?q=spec+dsf+hfgh++gfh"
      getURL (" " <> u <> " ") `shouldEqual` Just u
    it "URL 3 https://pursuit.purescript.org/search?q=spec+dsf+hfgh++gfh -> URL" do
      let
        u = "https://pursuit.purescript.org/search?q=spec+dsf+hfgh++gfh"
      getURL (" fasdf" <> u <> " fdah fdh") `shouldEqual` Just u
