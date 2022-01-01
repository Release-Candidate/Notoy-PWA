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
  ( decodedEncodedURLs
  , spec
  ) where

import Prelude
import Control.Monad.Error.Class (class MonadThrow)
import Data.Foldable (for_)
import Data.Interpolate (interp)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Effect.Exception (Error)
import Helpers.General (decodeURIString, decodeURIStringMaybe, decodeURLString, decodeURLStringMaybe, encodeURIString, encodeURIStringMaybe, encodeURLString, encodeURLStringMaybe, getFirstMatch, getURL)
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
    decodeEncodeURLSpecs
    decodeEncodeURISpecs
    decodeEncodeURLMaybeSpecs
    decodeEncodeURIMaybeSpecs

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
    for_ decodedEncodedURLs \(Tuple _ e) -> getURLHelper "gfdgds" e " hgdfg" $ Just e
    for_ decodedEncodedURIs \(Tuple _ e) -> getURLHelper "gfdgds" e " hgdfg" $ Just e
    for_ urlsInvalid \u -> getURLHelper "" u "" Nothing

getURLHelper ::
  forall m a.
  Monad m =>
  MonadThrow Error a =>
  String -> String -> String -> Maybe String -> SpecT a Unit m Unit
getURLHelper p1 url p2 result =
  it (interp "Text '" p1 url p2 "' -> " $ show result) do
    getURL (p1 <> url <> p2) `shouldEqual` (fromAbsolute =<< result)

url1 :: String
url1 = "https://pursuit.purescript.org/search?q=spec+dsf+hfgh++gfh"

{-------------------------------------------------------------------------------
| Tests for functions `decodeURLString` and `encodeURLString`.
-}
decodeEncodeURLSpecs :: Spec Unit
decodeEncodeURLSpecs =
  describe "decodeURLString - encodeURLString" do
    for_ decodedEncodedURLs \(Tuple d e) -> decodedEncodeURLHelper "decode" decodeURLString e d
    for_ decodedEncodedURLs \(Tuple d e) -> decodedEncodeURLHelper "encode" encodeURLString d e
    for_ decodedEncodedURLs \(Tuple _ e) -> decodedEncodeURLHelper "encode ° decode" (encodeURLString <<< decodeURLString) e e
    for_ decodedEncodedURLs \(Tuple d _) -> decodedEncodeURLHelper "decode ° encode" (decodeURLString <<< encodeURLString) d d

decodedEncodeURLHelper ::
  forall m a.
  Monad m =>
  MonadThrow Error a => String -> (String -> String) -> String -> String -> SpecT a Unit m Unit
decodedEncodeURLHelper title f argument result =
  it (interp title " URL: " argument) do
    f argument `shouldEqual` result

{-------------------------------------------------------------------------------
| Tests for functions `decodeURIString` and `encodeURIString`.
-}
decodeEncodeURISpecs :: Spec Unit
decodeEncodeURISpecs =
  describe "decodeURIString - encodeURIString" do
    for_ decodedEncodedURIs \(Tuple d e) -> decodedEncodeURIHelper "decode" decodeURIString e d
    for_ decodedEncodedURIs \(Tuple d e) -> decodedEncodeURIHelper "encode" encodeURIString d e
    for_ decodedEncodedURIs \(Tuple _ e) -> decodedEncodeURIHelper "encode ° decode" (encodeURIString <<< decodeURIString) e e
    for_ decodedEncodedURIs \(Tuple d _) -> decodedEncodeURIHelper "decode ° encode" (decodeURIString <<< encodeURIString) d d

decodedEncodeURIHelper ::
  forall m a.
  Monad m =>
  MonadThrow Error a =>
  String -> (String -> String) -> String -> String -> SpecT a Unit m Unit
decodedEncodeURIHelper title f argument result =
  it (interp title " URI: " argument) do
    f argument `shouldEqual` result

{-------------------------------------------------------------------------------
| Tests for functions `decodeURLString` and `encodeURLString`.
-}
decodeEncodeURLMaybeSpecs :: Spec Unit
decodeEncodeURLMaybeSpecs =
  describe "decodeURLStringMaybe - encodeURLStringMaybe" do
    decodedEncodeURLMaybeHelper "decode" decodeURLStringMaybe Nothing Nothing
    decodedEncodeURLMaybeHelper "encode" encodeURLStringMaybe Nothing Nothing
    decodedEncodeURLMaybeHelper "encode ° decode" (encodeURLStringMaybe <<< decodeURLStringMaybe) Nothing Nothing
    decodedEncodeURLMaybeHelper "decode ° encode" (decodeURLStringMaybe <<< encodeURLStringMaybe) Nothing Nothing
    for_ decodedEncodedURLs \(Tuple d e) -> decodedEncodeURLMaybeHelper "decode" decodeURLStringMaybe (Just e) (Just d)
    for_ decodedEncodedURLs \(Tuple d e) -> decodedEncodeURLMaybeHelper "encode" encodeURLStringMaybe (Just d) (Just e)
    for_ decodedEncodedURLs \(Tuple _ e) -> decodedEncodeURLMaybeHelper "encode ° decode" (encodeURLStringMaybe <<< decodeURLStringMaybe) (Just e) (Just e)
    for_ decodedEncodedURLs \(Tuple d _) -> decodedEncodeURLMaybeHelper "decode ° encode" (decodeURLStringMaybe <<< encodeURLStringMaybe) (Just d) (Just d)

decodedEncodeURLMaybeHelper ::
  forall a m.
  Monad m =>
  MonadThrow Error a =>
  String -> (Maybe String -> Maybe String) -> Maybe String -> Maybe String -> SpecT a Unit m Unit
decodedEncodeURLMaybeHelper title f argument result =
  it (interp title " URL: " (show argument)) do
    f argument `shouldEqual` result

{-------------------------------------------------------------------------------
| Tests for functions `decodeURIString` and `encodeURIString`.
-}
decodeEncodeURIMaybeSpecs :: Spec Unit
decodeEncodeURIMaybeSpecs =
  describe "decodeURIStringMaybe - encodeURIStringMaybe" do
    decodedEncodeURIMaybeHelper "decode" decodeURIStringMaybe Nothing Nothing
    decodedEncodeURIMaybeHelper "encode" encodeURIStringMaybe Nothing Nothing
    decodedEncodeURIMaybeHelper "encode ° decode" (encodeURIStringMaybe <<< decodeURIStringMaybe) Nothing Nothing
    decodedEncodeURIMaybeHelper "decode ° encode" (decodeURIStringMaybe <<< encodeURIStringMaybe) Nothing Nothing
    for_ decodedEncodedURIs \(Tuple d e) -> decodedEncodeURIMaybeHelper "decode" decodeURIStringMaybe (Just e) (Just d)
    for_ decodedEncodedURIs \(Tuple d e) -> decodedEncodeURIMaybeHelper "encode" encodeURIStringMaybe (Just d) (Just e)
    for_ decodedEncodedURIs \(Tuple _ e) -> decodedEncodeURIMaybeHelper "encode ° decode" (encodeURIStringMaybe <<< decodeURIStringMaybe) (Just e) (Just e)
    for_ decodedEncodedURIs \(Tuple d _) -> decodedEncodeURIMaybeHelper "decode ° encode" (decodeURIStringMaybe <<< encodeURIStringMaybe) (Just d) (Just d)

decodedEncodeURIMaybeHelper ::
  forall a m.
  Monad m =>
  MonadThrow Error a =>
  String -> (Maybe String -> Maybe String) -> Maybe String -> Maybe String -> SpecT a Unit m Unit
decodedEncodeURIMaybeHelper title f argument result =
  it (interp title " URI: " (show argument)) do
    f argument `shouldEqual` result

decodedEncodedURLs :: Array (Tuple String String)
decodedEncodedURLs =
  [ Tuple "https://pursuit.purescript.org/search?q=spec dsf hfgh  gfh" "https%3A%2F%2Fpursuit.purescript.org%2Fsearch%3Fq%3Dspec+dsf+hfgh++gfh"
  , Tuple "https://www.bla.com:1234/ sdf fsd" "https%3A%2F%2Fwww.bla.com%3A1234%2F+sdf+fsd"
  , Tuple "http://✪df.ws/123" "http%3A%2F%2F%E2%9C%AAdf.ws%2F123"
  , Tuple "https://localhost:1234" "https%3A%2F%2Flocalhost%3A1234"
  , Tuple "https://localhost" "https%3A%2F%2Flocalhost"
  , Tuple "http://userid:password@example.com:8080" "http%3A%2F%2Fuserid%3Apassword%40example.com%3A8080"
  , Tuple "http://userid:password@example.com:8080/" "http%3A%2F%2Fuserid%3Apassword%40example.com%3A8080%2F"
  , Tuple "http://userid@example.com" "http%3A%2F%2Fuserid%40example.com"
  , Tuple "http://userid@example.com/" "http%3A%2F%2Fuserid%40example.com%2F"
  , Tuple "http://userid@example.com:8080" "http%3A%2F%2Fuserid%40example.com%3A8080"
  , Tuple "http://userid@example.com:8080/" "http%3A%2F%2Fuserid%40example.com%3A8080%2F"
  , Tuple "http://userid:password@example.com" "http%3A%2F%2Fuserid%3Apassword%40example.com"
  , Tuple "http://userid:password@example.com/" "http%3A%2F%2Fuserid%3Apassword%40example.com%2F"
  , Tuple "http://142.42.1.1/" "http%3A%2F%2F142.42.1.1%2F"
  , Tuple "http://142.42.1.1:8080/" "http%3A%2F%2F142.42.1.1%3A8080%2F"
  , Tuple "http://➡.ws/䨹" "http%3A%2F%2F%E2%9E%A1.ws%2F%E4%A8%B9"
  , Tuple "http://⌘.ws" "http%3A%2F%2F%E2%8C%98.ws"
  , Tuple "http://⌘.ws/" "http%3A%2F%2F%E2%8C%98.ws%2F"
  , Tuple "http://foo.com/blah_(wikipedia)#cite-1" "http%3A%2F%2Ffoo.com%2Fblah_%28wikipedia%29%23cite-1"
  , Tuple "http://foo.com/blah_(wikipedia)_blah#cite-1" "http%3A%2F%2Ffoo.com%2Fblah_%28wikipedia%29_blah%23cite-1"
  , Tuple "http://foo.com/unicode_(✪)_in_parens" "http%3A%2F%2Ffoo.com%2Funicode_%28%E2%9C%AA%29_in_parens"
  , Tuple "http://foo.com/(something)?after=parens" "http%3A%2F%2Ffoo.com%2F%28something%29%3Fafter%3Dparens"
  , Tuple "http://☺.damowmow.com/" "http%3A%2F%2F%E2%98%BA.damowmow.com%2F"
  , Tuple "http://code.google.com/events/#&product=browser" "http%3A%2F%2Fcode.google.com%2Fevents%2F%23%26product%3Dbrowser"
  , Tuple "http://j.mp" "http%3A%2F%2Fj.mp"
  , Tuple "ftp://foo.bar/baz" "ftp%3A%2F%2Ffoo.bar%2Fbaz"
  , Tuple "http://www.foo.bar./" "http%3A%2F%2Fwww.foo.bar.%2F"
  , Tuple "http://foo.bar/?q=Test%20URL-encoded%20stuff" "http%3A%2F%2Ffoo.bar%2F%3Fq%3DTest%2520URL-encoded%2520stuff"
  , Tuple "http://مثال.إختبار" "http%3A%2F%2F%D9%85%D8%AB%D8%A7%D9%84.%D8%A5%D8%AE%D8%AA%D8%A8%D8%A7%D8%B1"
  , Tuple "http://例子.测试" "http%3A%2F%2F%E4%BE%8B%E5%AD%90.%E6%B5%8B%E8%AF%95"
  , Tuple "http://10.1.1.0" "http%3A%2F%2F10.1.1.0"
  , Tuple "http://10.1.1.255" "http%3A%2F%2F10.1.1.255"
  , Tuple "http://224.1.1.1" "http%3A%2F%2F224.1.1.1"
  , Tuple "http://1.1.1.1.1" "http%3A%2F%2F1.1.1.1.1"
  , Tuple "http://123.123.123" "http%3A%2F%2F123.123.123"
  ]

decodedEncodedURIs :: Array (Tuple String String)
decodedEncodedURIs =
  [ Tuple "https://pursuit.purescript.org/search?q=spec dsf hfgh  gfh" "https%3A%2F%2Fpursuit.purescript.org%2Fsearch%3Fq%3Dspec%20dsf%20hfgh%20%20gfh"
  , Tuple "https://www.bla.com:1234/ sdf fsd" "https%3A%2F%2Fwww.bla.com%3A1234%2F%20sdf%20fsd"
  , Tuple "http://✪df.ws/123" "http%3A%2F%2F%E2%9C%AAdf.ws%2F123"
  , Tuple "https://localhost:1234" "https%3A%2F%2Flocalhost%3A1234"
  , Tuple "https://localhost" "https%3A%2F%2Flocalhost"
  , Tuple "http://userid:password@example.com:8080" "http%3A%2F%2Fuserid%3Apassword%40example.com%3A8080"
  , Tuple "http://userid:password@example.com:8080/" "http%3A%2F%2Fuserid%3Apassword%40example.com%3A8080%2F"
  , Tuple "http://userid@example.com" "http%3A%2F%2Fuserid%40example.com"
  , Tuple "http://userid@example.com/" "http%3A%2F%2Fuserid%40example.com%2F"
  , Tuple "http://userid@example.com:8080" "http%3A%2F%2Fuserid%40example.com%3A8080"
  , Tuple "http://userid@example.com:8080/" "http%3A%2F%2Fuserid%40example.com%3A8080%2F"
  , Tuple "http://userid:password@example.com" "http%3A%2F%2Fuserid%3Apassword%40example.com"
  , Tuple "http://userid:password@example.com/" "http%3A%2F%2Fuserid%3Apassword%40example.com%2F"
  , Tuple "http://142.42.1.1/" "http%3A%2F%2F142.42.1.1%2F"
  , Tuple "http://142.42.1.1:8080/" "http%3A%2F%2F142.42.1.1%3A8080%2F"
  , Tuple "http://➡.ws/䨹" "http%3A%2F%2F%E2%9E%A1.ws%2F%E4%A8%B9"
  , Tuple "http://⌘.ws" "http%3A%2F%2F%E2%8C%98.ws"
  , Tuple "http://⌘.ws/" "http%3A%2F%2F%E2%8C%98.ws%2F"
  , Tuple "http://foo.com/blah_(wikipedia)#cite-1" "http%3A%2F%2Ffoo.com%2Fblah_%28wikipedia%29%23cite-1"
  , Tuple "http://foo.com/blah_(wikipedia)_blah#cite-1" "http%3A%2F%2Ffoo.com%2Fblah_%28wikipedia%29_blah%23cite-1"
  , Tuple "http://foo.com/unicode_(✪)_in_parens" "http%3A%2F%2Ffoo.com%2Funicode_%28%E2%9C%AA%29_in_parens"
  , Tuple "http://foo.com/(something)?after=parens" "http%3A%2F%2Ffoo.com%2F%28something%29%3Fafter%3Dparens"
  , Tuple "http://☺.damowmow.com/" "http%3A%2F%2F%E2%98%BA.damowmow.com%2F"
  , Tuple "http://code.google.com/events/#&product=browser" "http%3A%2F%2Fcode.google.com%2Fevents%2F%23%26product%3Dbrowser"
  , Tuple "http://j.mp" "http%3A%2F%2Fj.mp"
  , Tuple "ftp://foo.bar/baz" "ftp%3A%2F%2Ffoo.bar%2Fbaz"
  , Tuple "http://www.foo.bar./" "http%3A%2F%2Fwww.foo.bar.%2F"
  , Tuple "http://foo.bar/?q=Test%20URL-encoded%20stuff" "http%3A%2F%2Ffoo.bar%2F%3Fq%3DTest%2520URL-encoded%2520stuff"
  , Tuple "http://مثال.إختبار" "http%3A%2F%2F%D9%85%D8%AB%D8%A7%D9%84.%D8%A5%D8%AE%D8%AA%D8%A8%D8%A7%D8%B1"
  , Tuple "http://例子.测试" "http%3A%2F%2F%E4%BE%8B%E5%AD%90.%E6%B5%8B%E8%AF%95"
  , Tuple "http://10.1.1.0" "http%3A%2F%2F10.1.1.0"
  , Tuple "http://10.1.1.255" "http%3A%2F%2F10.1.1.255"
  , Tuple "http://224.1.1.1" "http%3A%2F%2F224.1.1.1"
  , Tuple "http://1.1.1.1.1" "http%3A%2F%2F1.1.1.1.1"
  , Tuple "http://123.123.123" "http%3A%2F%2F123.123.123"
  ]

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
