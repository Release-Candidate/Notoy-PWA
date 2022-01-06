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
import Control.Monad.Error.Class (class MonadThrow)
import Data.Argonaut (decodeJson, encodeJson, stringify)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Interpolate (interp)
import Data.Maybe (Maybe(..))
import Data.Note (Note(..), fromShared)
import Data.String (trim)
import Data.Tuple (Tuple(..))
import Data.URL (noteUrlFromString)
import Effect.Exception (Error)
import Helpers.General (decodeJsonFromString, encodeToJsonString)
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
  describe "Data.Note - Tests" do
    fromSharedSpecs
    jsonEncodeDecodeSpecs

{-------------------------------------------------------------------------------
| Tests for the function `fromShared`.
-}
fromSharedSpecs :: Spec Unit
fromSharedSpecs =
  describe "fromShared" do
    for_ fromSharedNotes \(Tuple note result) ->
      fromSharedHelper note result
    it "Quickcheck Strings"
      $ quickCheck \s1 s2 ->
          let
            note =
              Note
                { title: Just s1
                , url: noteUrlFromString "http://url"
                , keywords: Nothing
                , shortDesc: Just s2
                , longDesc: Nothing
                }
          in
            fromShared (Just s1) (noteUrlFromString "http://url") (Just s2) === note
    it "Quickcheck Strings + URL in Title"
      $ quickCheck \s1 s2 ->
          let
            urlSt = case s1 of
              "" -> Nothing
              _ -> Just $ trim $ s1 <> "http://url"

            note =
              Note
                { title: urlSt
                , url: noteUrlFromString "http://url"
                , keywords: Nothing
                , shortDesc: Just $ trim $ s2
                , longDesc: Nothing
                }
          in
            fromShared (Just $ s1 <> "http://url") Nothing (Just s2) === note
    it "Quickcheck Strings + URL in Desc"
      $ quickCheck \s1 s2 ->
          let
            urlSt = case s1 of
              "" -> Nothing
              _ -> Just $ trim $ s1 <> "http://url"

            note =
              Note
                { title: Just $ trim $ s2
                , url: noteUrlFromString "http://url"
                , keywords: Nothing
                , shortDesc: urlSt
                , longDesc: Nothing
                }
          in
            fromShared (Just s2) Nothing (Just $ s1 <> "http://url") === note
    it "Quickcheck double Strings + URL in Title"
      $ quickCheck \s1 s2 s3 ->
          let
            urlSt = case s1 <> s3 of
              "" -> Nothing
              _ -> Just $ trim $ s1 <> "http://url" <> " " <> s3

            note =
              Note
                { title: urlSt
                , url: noteUrlFromString "http://url"
                , keywords: Nothing
                , shortDesc: Just $ trim $ s2
                , longDesc: Nothing
                }
          in
            fromShared (Just $ s1 <> "http://url" <> " " <> s3) Nothing (Just s2) === note
    it "Quickcheck Strings + URL in Desc"
      $ quickCheck \s1 s2 s3 ->
          let
            urlSt = case s1 <> s3 of
              "" -> Nothing
              _ -> Just $ trim $ s1 <> "http://url" <> " " <> s3

            note =
              Note
                { title: Just $ trim $ s2
                , url: noteUrlFromString "http://url"
                , keywords: Nothing
                , shortDesc: urlSt
                , longDesc: Nothing
                }
          in
            fromShared (Just s2) Nothing (Just $ s1 <> "http://url" <> " " <> s3) === note
    it "Quickcheck whole Note"
      $ quickCheck \note ->
          let
            Note
              { title: title
            , url: url
            , keywords: _
            , shortDesc: shortDesc
            , longDesc: _
            } = note

            note1 =
              Note
                { title: title
                , url: url
                , keywords: Nothing
                , shortDesc: shortDesc
                , longDesc: Nothing
                }
          in
            fromShared title url shortDesc === note1

fromSharedHelper ::
  forall m a.
  Monad m =>
  MonadThrow Error a =>
  Note -> Note -> SpecT a Unit m Unit
fromSharedHelper note result =
  it (interp "Note: '" (show note) "' -> '" (show result) "'") do
    let
      Note
        { title: title
      , url: url
      , shortDesc: shortDesc
      , longDesc: _
      } = note
    fromShared title url shortDesc `shouldEqual` result

{-------------------------------------------------------------------------------
| Tests of the JSON de- and encoding.
-}
jsonEncodeDecodeSpecs :: Spec Unit
jsonEncodeDecodeSpecs =
  describe "JSON decoding and encoding" do
    for_ fromSharedNotes \(Tuple note _) ->
      it (interp "decodeJson ° encodeJson of " (stringify $ encodeJson note)) do
        (decodeJson $ encodeJson note) `shouldEqual` Right note
    it "Quickcheck decodeJson ° encodeJson"
      $ quickCheck \(note :: Note) ->
          (decodeJson $ encodeJson note) === Right note
    it "Quickcheck decodeJsonFromString ° encodeToJsonString"
      $ quickCheck \(note :: Note) ->
          (decodeJsonFromString $ encodeToJsonString note) === Right note

fromSharedNotes :: Array (Tuple Note Note)
fromSharedNotes =
  [ Tuple
      ( Note
          { title: Just "Title 1"
          , url: noteUrlFromString "https://url.com:12354/index.html"
          , keywords: Nothing
          , shortDesc: Just "Short text"
          , longDesc: Nothing
          }
      )
      ( Note
          { title: Just "Title 1"
          , url: noteUrlFromString "https://url.com:12354/index.html"
          , keywords: Nothing
          , shortDesc: Just "Short text"
          , longDesc: Nothing
          }
      )
  , Tuple
      ( Note
          { title: Just "Title 1"
          , url: noteUrlFromString "\"https://url.com:12354/index.html\""
          , keywords: Nothing
          , shortDesc: Just "Short text"
          , longDesc: Nothing
          }
      )
      ( Note
          { title: Just "Title 1"
          , url: noteUrlFromString "https://url.com:12354/index.html"
          , keywords: Nothing
          , shortDesc: Just "Short text"
          , longDesc: Nothing
          }
      )
  , Tuple
      ( Note
          { title: Just "Title 2"
          , url: Nothing
          , keywords: Nothing
          , shortDesc: Just "Short text 2"
          , longDesc: Nothing
          }
      )
      ( Note
          { title: Just "Title 2"
          , url: Nothing
          , keywords: Nothing
          , shortDesc: Just "Short text 2"
          , longDesc: Nothing
          }
      )
  , Tuple
      ( Note
          { title: Just "Title 3"
          , url: Nothing
          , keywords: Nothing
          , shortDesc: Nothing
          , longDesc: Nothing
          }
      )
      ( Note
          { title: Just "Title 3"
          , url: Nothing
          , keywords: Nothing
          , shortDesc: Nothing
          , longDesc: Nothing
          }
      )
  , Tuple
      ( Note
          { title: Nothing
          , url: Nothing
          , keywords: Nothing
          , shortDesc: Just "Short text 4"
          , longDesc: Nothing
          }
      )
      ( Note
          { title: Nothing
          , url: Nothing
          , keywords: Nothing
          , shortDesc: Just "Short text 4"
          , longDesc: Nothing
          }
      )
  , Tuple
      ( Note
          { title: Nothing
          , url: Nothing
          , keywords: Nothing
          , shortDesc: Nothing
          , longDesc: Nothing
          }
      )
      ( Note
          { title: Nothing
          , url: Nothing
          , keywords: Nothing
          , shortDesc: Nothing
          , longDesc: Nothing
          }
      )
  , Tuple
      ( Note
          { title: Just "http://url"
          , url: Nothing
          , keywords: Nothing
          , shortDesc: Nothing
          , longDesc: Nothing
          }
      )
      ( Note
          { title: Nothing
          , url: noteUrlFromString "http://url"
          , keywords: Nothing
          , shortDesc: Nothing
          , longDesc: Nothing
          }
      )
  , Tuple
      ( Note
          { title: Just "\"http://url"
          , url: Nothing
          , keywords: Nothing
          , shortDesc: Nothing
          , longDesc: Nothing
          }
      )
      ( Note
          { title: Nothing
          , url: noteUrlFromString "http://url"
          , keywords: Nothing
          , shortDesc: Nothing
          , longDesc: Nothing
          }
      )
  , Tuple
      ( Note
          { title: Just "\"http://url\""
          , url: Nothing
          , keywords: Nothing
          , shortDesc: Nothing
          , longDesc: Nothing
          }
      )
      ( Note
          { title: Nothing
          , url: noteUrlFromString "http://url"
          , keywords: Nothing
          , shortDesc: Nothing
          , longDesc: Nothing
          }
      )
  , Tuple
      ( Note
          { title: Just "http://url\""
          , url: Nothing
          , keywords: Nothing
          , shortDesc: Nothing
          , longDesc: Nothing
          }
      )
      ( Note
          { title: Nothing
          , url: noteUrlFromString "http://url"
          , keywords: Nothing
          , shortDesc: Nothing
          , longDesc: Nothing
          }
      )
  , Tuple
      ( Note
          { title: Nothing
          , url: Nothing
          , keywords: Nothing
          , shortDesc: Just "http://url"
          , longDesc: Nothing
          }
      )
      ( Note
          { title: Nothing
          , url: noteUrlFromString "http://url"
          , keywords: Nothing
          , shortDesc: Nothing
          , longDesc: Nothing
          }
      )
  , Tuple
      ( Note
          { title: Just " http://url "
          , url: Nothing
          , keywords: Nothing
          , shortDesc: Nothing
          , longDesc: Nothing
          }
      )
      ( Note
          { title: Nothing
          , url: noteUrlFromString "http://url"
          , keywords: Nothing
          , shortDesc: Nothing
          , longDesc: Nothing
          }
      )
  , Tuple
      ( Note
          { title: Nothing
          , url: Nothing
          , keywords: Nothing
          , shortDesc: Just " http://url "
          , longDesc: Nothing
          }
      )
      ( Note
          { title: Nothing
          , url: noteUrlFromString "http://url"
          , keywords: Nothing
          , shortDesc: Nothing
          , longDesc: Nothing
          }
      )
  , Tuple
      ( Note
          { title: Just "http://url fgsdf"
          , url: Nothing
          , keywords: Nothing
          , shortDesc: Nothing
          , longDesc: Nothing
          }
      )
      ( Note
          { title: Just "http://url fgsdf"
          , url: noteUrlFromString "http://url"
          , keywords: Nothing
          , shortDesc: Nothing
          , longDesc: Nothing
          }
      )
  , Tuple
      ( Note
          { title: Nothing
          , url: Nothing
          , keywords: Nothing
          , shortDesc: Just " http://url ghgfdgh "
          , longDesc: Nothing
          }
      )
      ( Note
          { title: Nothing
          , url: noteUrlFromString "http://url"
          , keywords: Nothing
          , shortDesc: Just "http://url ghgfdgh"
          , longDesc: Nothing
          }
      )
  ]
