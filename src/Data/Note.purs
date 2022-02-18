-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     Note.purs
-- Date:     23.Dec.2021
--
-- =============================================================================
-- | Module Data.Note, module holding functions and records about the note data.
module Data.Note
  ( KeyWordArray(..)
  , Note(..)
  , defaultNote
  , fromShared
  , keyWordArrayFromString
  , noteKeyId
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Array (foldl)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.StoreKey (class StoreKey, StoreKeyId(..))
import Data.String (Pattern(..), split, trim)
import Data.Tuple (Tuple(..))
import Data.URL (NoteURL, noteUrlToString, trimQuotes)
import Helpers.General (getNoteURL, getURLString)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)

{-------------------------------------------------------------------------------
| The `StoreKeyId` of a `Note`.
-}
noteKeyId :: StoreKeyId
noteKeyId = StoreKeyId "Note"

{-------------------------------------------------------------------------------
| The actual data and text of the note.
|
| Any of these may be missing (`Nothing`).
| * `title` - the title of the note.
| * `url` - the URL of the website the note is about.
|           ATTENTION: has to be encoded to work as a link!
| * `keywords` - the array of keywords that describe the note
| * `location` - the location the note is taken
| * `shortDesc` - the short description text.
| * `longDesc` - the longer, detailed description.
-}
newtype Note
  = Note
  { title :: Maybe String
  , url :: Maybe NoteURL
  , keywords :: Maybe KeyWordArray
  , location :: Maybe String
  , shortDesc :: Maybe String
  , longDesc :: Maybe String
  }

{-------------------------------------------------------------------------------
| The default, empty note.
-}
defaultNote :: Note
defaultNote =
  Note
    { title: Nothing
    , url: Nothing
    , keywords: Nothing
    , location: Nothing
    , shortDesc: Nothing
    , longDesc: Nothing
    }

derive instance eqNote :: Eq Note

derive instance genericNote :: Generic Note _

instance encodeJSONNote :: EncodeJson Note where
  encodeJson = genericEncodeJson

instance decodeJSONNote :: DecodeJson Note where
  decodeJson = genericDecodeJson

instance arbitraryNote :: Arbitrary Note where
  arbitrary = genericArbitrary

instance storeKeyNote :: StoreKey Note where
  key _ = noteKeyId

instance showNote :: Show Note where
  show (Note note) =
    titleString
      <> urlString
      <> keywordString
      <> locationString
      <> shortString
      <> longString
    where
    showField :: String -> Maybe String -> String
    showField name val = case val of
      Just s -> name <> ": " <> s <> " "
      Nothing -> ""

    showFieldKeyWds keywds = case keywds of
      Just keys -> "Keywords: " <> show keys <> " "
      Nothing -> ""

    titleString = showField "Title" note.title

    urlString = showField "URL" $ map noteUrlToString note.url

    keywordString = showFieldKeyWds note.keywords

    locationString = showField "Location" note.location

    shortString = showField "Short Description" note.shortDesc

    longString = showField "Detailed Description" note.longDesc

{-------------------------------------------------------------------------------
| Construct a `Note` from the given title, url and text.
|
| Checks the URL and if it is empty, searches in the `text` and `title` argument
| for an URL.
|
| * `title` - The title of the shared link.
| * `url` - The URL of the shared link - this is empty on Android, the url is
|           passed in `text`.
| * `text` - The description of the link. This holds the link's URL on Android.
-}
fromShared :: Maybe String -> Maybe NoteURL -> Maybe String -> Note
fromShared (Just title) Nothing Nothing =
  Note
    { title: txt
    , url: urlSt
    , keywords: Nothing
    , location: Nothing
    , shortDesc: Nothing
    , longDesc: Nothing
    }
  where
  UrlString (Tuple urlSt txt) = getURLAndText title

fromShared Nothing Nothing (Just text) =
  Note
    { title: Nothing
    , url: urlSt
    , keywords: Nothing
    , location: Nothing
    , shortDesc: txt
    , longDesc: Nothing
    }
  where
  UrlString (Tuple urlSt txt) = getURLAndText text

fromShared (Just title) Nothing (Just text) =
  Note
    { title: tl
    , url: foundURL
    , keywords: Nothing
    , location: Nothing
    , shortDesc: txt
    , longDesc: Nothing
    }
  where
  UrlString (Tuple foundURL1 txt) = getURLAndText text

  UrlString (Tuple foundURL2 tl) = getURLAndText title

  foundURL = case foundURL1 of
    Nothing -> foundURL2
    _ -> foundURL1

fromShared title url text =
  Note
    { title: map trim title
    , url: url
    , keywords: Nothing
    , location: Nothing
    , shortDesc: map trim text
    , longDesc: Nothing
    }

{-------------------------------------------------------------------------------
| Type for holding an array of keywords.
-}
newtype KeyWordArray
  = KeyWordArray (Array String)

derive instance eqKeywordArray :: Eq KeyWordArray

derive instance ordKeyWordArray :: Ord KeyWordArray

derive instance genericKeyWordArray :: Generic KeyWordArray _

instance encodeJSONKeyWordArray :: EncodeJson KeyWordArray where
  encodeJson = genericEncodeJson

instance decodeJSONKeyWordArray :: DecodeJson KeyWordArray where
  decodeJson = genericDecodeJson

instance arbitraryKeyWordArray :: Arbitrary KeyWordArray where
  arbitrary = genericArbitrary

instance showKeyWordArray :: Show KeyWordArray where
  show (KeyWordArray keys) =
    foldl
      ( \acc e ->
          if acc == "" then
            acc <> e
          else
            acc <> ", " <> e
      )
      ""
      keys

{-------------------------------------------------------------------------------
| Convert a string of comma separated keywords to a `KeyWordArray`.
|
| Return `Nothing` if the string is empty or only whitespace.
|
| * `str` - The string of comma separated keywords.
-}
keyWordArrayFromString :: String -> Maybe KeyWordArray
keyWordArrayFromString "" = Nothing

keyWordArrayFromString str
  | trim str == "" = Nothing
  | otherwise = Just $ KeyWordArray $ map trim $ split (Pattern ",") str

{-------------------------------------------------------------------------------
| Helper: Type to hold a Tuple of the parsed URL and text (URL, text), both as a
| `Maybe String`.
|
| The URL is `Nothing`, if no URL could be found in `text`.
|
| The text is `Nothing`, if it only consisted of the URL (and whitespace).
-}
newtype UrlString
  = UrlString (Tuple (Maybe NoteURL) (Maybe String))

{-------------------------------------------------------------------------------
| Helper: try to find a URL in the given string.
|
| If an URL is found in the given string and the string is the URL with
| whitespace added, the text part of the tuple is `Nothing`.
-}
getURLAndText :: String -> UrlString
getURLAndText text = UrlString (Tuple url txt)
  where
  trimmed = Just $ trimQuotes $ trim text

  url = getNoteURL text

  urlSt = getURLString $ trimQuotes text

  txt = case trimmed == urlSt of
    true -> Nothing
    _ -> trimmed
