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
    getURLHelper "" url1 "" $ Just url1
    getURLHelper " " url1 " " $ Just url1
    getURLHelper " fasdf" url1 " fdah fdh" $ Just url1
    it (interp "Quickcheck URLs " url1 " -> URL")
      $ quickCheck \s1 s2 -> getURL (s1 <> url1 <> " " <> s2) === Just url1
    for_ urlsValid \u -> getURLHelper " " u " " $ Just u
    for_ urlsInvalid \u -> getURLHelper " " u " " Nothing

getURLHelper ::
  forall m a.
  Monad m =>
  MonadThrow Error a =>
  String -> String -> String -> Maybe String -> SpecT a Unit m Unit
getURLHelper p1 url p2 result =
  it (interp "Text '" p1 url p2 "' -> " $ show result) do
    getURL (p1 <> url <> p2) `shouldEqual` result

url1 ∷ String
url1 = "https://pursuit.purescript.org/search?q=spec+dsf+hfgh++gfh"

{-------------------------------------------------------------------------------
| This is a list of valid URLs by Alexey Zapparov from
| https://gist.github.com/dperini/729294#gistcomment-972393
| licensed under the MIT
-}
urlsValid :: Array String
urlsValid =
  [ "http://✪df.ws/123"
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
  , "http://foo.bar/?q=Test%20URL-encoded%20stuff"
  , "http://مثال.إختبار"
  , "http://例子.测试"
  ]

{-------------------------------------------------------------------------------
| This is a list of invalid URLs by Alexey Zapparov from
| https://gist.github.com/dperini/729294#gistcomment-972393
| licensed under the MIT
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
  , "http:// shouldfail.com"
  , ":// should fail"
  , "http://foo.bar/foo(bar)baz quux"
  , "ftps://foo.bar/"
  , "http://-error-.invalid/"
  , "http://a.b--c.de/"
  , "http://-a.b.co"
  , "http://a.b-.co"
  , "http://0.0.0.0"
  , "http://10.1.1.0"
  , "http://10.1.1.255"
  , "http://224.1.1.1"
  , "http://1.1.1.1.1"
  , "http://123.123.123"
  , "http://3628126748"
  , "http://.www.foo.bar/"
  , "http://www.foo.bar./"
  , "http://.www.foo.bar./"
  , "http://10.1.1.1"
  , "http://10.1.1.254"
  ]
