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
import Control.Monad.Error.Class (class MonadThrow)
import Data.Foldable (for_)
import Data.Interpolate (interp)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect.Exception (Error)
import Helpers.General (getFirstMatch, getURL)
import Test.QuickCheck ((===))
import Test.Spec (Spec, SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Web.URL (fromAbsolute)

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
    getFirstMatchHelper testRegex1 "Hugo" Nothing
    getFirstMatchHelper testRegex1 "AFooda" $ Just "Foo"
    getFirstMatchHelper testRegex1 "AFoodFooa" $ Just "Foo"
    getFirstMatchHelper testRegex2 "Hugo" Nothing
    getFirstMatchHelper testRegex2 "A Foo da" $ Just "Foo"
    getFirstMatchHelper testRegex2
      "重温习主席这些新年贺词，我们豪情万丈FoodFoo习近平讲党史故事 出版发行 拼出我要的未来"
      $ Just "FoodFoo习近平讲党史故事"

getFirstMatchHelper ::
  forall m a.
  Monad m =>
  MonadThrow Error a =>
  Regex -> String -> Maybe String -> SpecT a Unit m Unit
getFirstMatchHelper rex txt result =
  it (interp "Regex '" (show rex) "' String '" txt "' -> " $ show result) do
    getFirstMatch rex txt `shouldEqual` result

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
    getURLHelper "This is" "not" " an URL" Nothing
    it (interp "Quickcheck URLs " url1 " -> URL")
      $ quickCheck \s1 s2 -> getURL (s1 <> url1 <> " " <> s2) === fromAbsolute url1
    for_ urlsValid \u -> getURLHelper "" u "" $ Just u
    for_ urlsValid \u -> getURLHelper " " u " " $ Just u
    for_ urlsValid \u -> getURLHelper "gfdgds" u " hgdfg" $ Just u
    for_ urlsInvalid \u -> getURLHelper "" u "" Nothing

url1 :: String
url1 = "https://pursuit.purescript.org/search?q=spec+dsf+hfgh++gfh"

getURLHelper ::
  forall m a.
  Monad m =>
  MonadThrow Error a =>
  String -> String -> String -> Maybe String -> SpecT a Unit m Unit
getURLHelper p1 url p2 result =
  it (interp "Text '" p1 url p2 "' -> " $ show result) do
    getURL (p1 <> url <> p2) `shouldEqual` (fromAbsolute =<< result)

{-------------------------------------------------------------------------------
| This is a list of valid and invalid URLs by Alexey Zapparov from
| https://gist.github.com/dperini/729294#gistcomment-972393
| licensed under the MIT license.
| Changed by me to get a list of strings that are almost a valid URL.
-}
urlsValid :: Array String
urlsValid =
  [ "https://pursuit.purescript.org/search?q=spec+dsf+hfgh++gfh"
  , "http://✪df.ws/123"
  , "https://localhost:1234"
  , "https://localhost"
  , "http://userid:password@example.com:8080"
  , "http://userid:password@example.com:8080/"
  , "http://userid@example.com"
  , "http://userid@example.com/"
  , "http://userid@example.com:8080"
  , "http://userid@example.com:8080/"
  , "http://userid:password@example.com"
  , "http://userid:password@example.com/"
  , "http://142.42.1.1/"
  , "http://142.42.1.1:8080/"
  , "http://➡.ws/䨹"
  , "http://⌘.ws"
  , "http://⌘.ws/"
  , "http://foo.com/blah_(wikipedia)#cite-1"
  , "http://foo.com/blah_(wikipedia)_blah#cite-1"
  , "http://foo.com/unicode_(✪)_in_parens"
  , "http://foo.com/(something)?after=parens"
  , "http://☺.damowmow.com/"
  , "http://code.google.com/events/#&product=browser"
  , "http://j.mp"
  , "ftp://foo.bar/baz"
  , "http://www.foo.bar./"
  , "http://foo.bar/?q=Test%20URL-encoded%20stuff"
  , "http://مثال.إختبار"
  , "http://例子.测试"
  , "http://10.1.1.0"
  , "http://10.1.1.255"
  , "http://224.1.1.1"
  , "http://1.1.1.1.1"
  , "http://123.123.123"
  ]

{-------------------------------------------------------------------------------
| This is a list of valid and invalid URLs by Alexey Zapparov from
| https://gist.github.com/dperini/729294#gistcomment-972393
| licensed under the MIT license.
| Changed by me to get a list of strings that are almost a valid URL.
-}
urlsInvalid :: Array String
urlsInvalid =
  [ "http://"
  , "http://."
  , "http://.."
  , "http://../"
  , "http://?"
  , "http://??"
  , "http://??/"
  , "http://#"
  , "http://##"
  , "http://##/"
  , "//"
  , "//a"
  , "///a"
  , "///"
  , "http:///a"
  , "foo.com"
  , "rdar://1234"
  , "h://test"
  , "ftps://foo.bar/"
  , "http://.www.foo.bar/"
  , "http://.www.foo.bar./"
  ]
