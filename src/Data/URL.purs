-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2022 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     URL.purs
-- Date:     02.Jan.2022
--
-- ==============================================================================
-- | Module Data.URL, the data type of an URL. Only needed to get JSON encoding
-- | and decoding.
module Data.URL
  ( NoteURL(..)
  , noteUrlFromString
  , noteUrlToString
  ) where

import Prelude
import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , JsonDecodeError(..)
  , decodeJson
  , encodeJson
  )
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Web.URL (unsafeFromAbsolute)
import Web.URL as WURL

newtype NoteURL
  = NoteURL WURL.URL

derive newtype instance eqURL :: Eq NoteURL

derive instance genericUrl :: Generic NoteURL _

derive newtype instance showUrl :: Show NoteURL

derive instance newTypeURL :: Newtype NoteURL _

{-------------------------------------------------------------------------------
| TODO: often returns "https://localhost:1234/"
-}
instance arbitraryNoteURL :: Arbitrary NoteURL where
  arbitrary = map stringToNoteURL arbitrary
    where
    stringToNoteURL :: String -> NoteURL
    stringToNoteURL s = case noteUrlFromString ("https://" <> s) of
      Nothing -> wrap $ unsafeFromAbsolute "https://localhost:1234/"
      Just url -> url

{-------------------------------------------------------------------------------
| Convert the given String to a `NoteURL`.
|
| * `s` - The String of the URL to convert to a `NoteURL`.
-}
noteUrlFromString :: String -> Maybe NoteURL
noteUrlFromString s = case (WURL.fromAbsolute s) of
  Nothing -> Nothing
  Just url -> Just $ wrap url

{-------------------------------------------------------------------------------
| Convert the given `NoteURL` to a String.
|
| * `url` - The `NoteURL` to convert to a String.
-}
noteUrlToString :: NoteURL -> String
noteUrlToString url = WURL.toString $ unwrap url

instance decodeJSONURL :: DecodeJson NoteURL where
  decodeJson json = do
    st <- decodeJson json
    note (TypeMismatch "URL") (noteUrlFromString st)

instance encodeJSONURL :: EncodeJson NoteURL where
  encodeJson url = encodeJson $ noteUrlToString url
