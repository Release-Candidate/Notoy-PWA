-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     General.purs
-- Date:     23.Dec.2021
--
-- =============================================================================
-- | Module Helpers.General, contains general helper functions.
module Helpers.General
  ( decodeJsonFromString
  , decodeURIString
  , decodeURIStringMaybe
  , decodeURLString
  , decodeURLStringMaybe
  , encodeToJsonString
  , encodeURIString
  , encodeURIStringMaybe
  , encodeURLString
  , encodeURLStringMaybe
  , getFirstMatch
  , getNoteURL
  , getURLString
  , regexNotAWord
  , regexURL
  , sanitizeFileName
  , showM
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonParser, printJsonDecodeError, stringify)
import Data.Array.NonEmpty (take)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Regex (Regex, match, replace, test)
import Data.String.Regex.Flags (RegexFlags(..), ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.URL (NoteURL, noteUrlFromString)
import JSURI (decodeFormURLComponent, decodeURIComponent, encodeFormURLComponent, encodeURIComponent)

{-------------------------------------------------------------------------------
| Regex to match everything that is not a unicode letter.
|
| Like whitespace, punctuation characters, ...
-}
regexNotAWord ∷ Regex
regexNotAWord =
  unsafeRegex "(\\P{L})+"
    $ RegexFlags
        { global: true
        , ignoreCase: true
        , unicode: true
        , dotAll: false
        , multiline: false
        , sticky: false
        }

{-------------------------------------------------------------------------------
| Replace all non unicode letters from the given string with an underline `_`.
|
| Replaces whitespace characters, punctuation characters, ...
|
| * `st` - The string to replace the non unicode letters of.
-}
sanitizeFileName ∷ String → String
sanitizeFileName = replace regexNotAWord "_"

{-------------------------------------------------------------------------------
| Regex to parse an URL.
|
| That is used to find an URL in text, not for validation.
-}
regexURL :: Regex
regexURL = unsafeRegex "(file|ftp|https?):\\/\\/[^\\s$.?#].[\\S]*[^\\s.]+" ignoreCase

{-------------------------------------------------------------------------------
| Return the first match of a regex in the given String, or `Nothing` if there
| is no match.
|
| * `rex` - The regex to use to parse the String.
| * `text` - The String to parse.
-}
getFirstMatch :: Regex -> String -> Maybe String
getFirstMatch rex text = firstMatch
  where
  firstMatch = case match rex text of
    Nothing -> Nothing
    Just matches -> case take 1 matches of
      [ frst ] -> frst
      _ -> Nothing

{-------------------------------------------------------------------------------
| Search for a URL in the given String and return that.
|
| If no URL has been found, `Nothing` is returned.
|
| * `text` - The String to parse for an URL.
-}
getNoteURL :: String -> Maybe NoteURL
getNoteURL text = case test regexURL text of
  false -> Nothing
  true -> noteUrlFromString =<< getFirstMatch regexURL text

{-------------------------------------------------------------------------------
| Search for an URL in the given String and return that.
|
| If no URL has been found, `Nothing` is returned.
|
| * `text` - The String to parse for an URL.
-}
getURLString :: String -> Maybe String
getURLString text = case test regexURL text of
  false -> Nothing
  true -> getFirstMatch regexURL text

{-------------------------------------------------------------------------------
| Decode a String according to `application/x-www-form-urlencoded`.
|
| Returns the empty String `""` on errors.
|
| * `text` - The URL-encoded String to decode.
-}
decodeURLString :: String -> String
decodeURLString = fromMaybe "" <<< decodeFormURLComponent

{-------------------------------------------------------------------------------
| Decode a String according to `RFC3896`.
|
| Returns the empty String `""` on errors.
|
| * `text` - The URI-encoded String to decode.
-}
decodeURIString :: String -> String
decodeURIString = fromMaybe "" <<< decodeURIComponent

{-------------------------------------------------------------------------------
| Encode a String according to `application/x-www-form-urlencoded`.
|
|  Returns the empty String `""` on errors.
|
| * `url` - The URL to encode.
-}
encodeURLString :: String -> String
encodeURLString = fromMaybe "" <<< encodeFormURLComponent

{-------------------------------------------------------------------------------
| Encode a String according to `RFC3896`.
|
|  Returns the empty String `""` on errors.
|
| * `uri` - The URL to encode.
-}
encodeURIString :: String -> String
encodeURIString = fromMaybe "" <<< encodeURIComponent

{-------------------------------------------------------------------------------
| Decode a String according to `application/x-www-form-urlencoded`.
|
| Returns `Nothing` on errors or if `text` is `Nothing`.
|
| * `text` - The URL-encoded String to decode.
-}
decodeURLStringMaybe :: Maybe String -> Maybe String
decodeURLStringMaybe text = decodeFormURLComponent =<< text

{-------------------------------------------------------------------------------
| Decode a String according to `RFC3896`.
|
| Returns `Nothing` on errors or if `text` is `Nothing`.
|
| * `text` - The URI-encoded String to decode.
-}
decodeURIStringMaybe :: Maybe String -> Maybe String
decodeURIStringMaybe text = decodeURIComponent =<< text

{-------------------------------------------------------------------------------
| Encode a String according to `application/x-www-form-urlencoded`.
|
| Returns `Nothing` on errors or if `text` is `Nothing`.
|
| * `text` - The URL-encoded String to decode.
-}
encodeURLStringMaybe :: Maybe String -> Maybe String
encodeURLStringMaybe text = encodeFormURLComponent =<< text

{-------------------------------------------------------------------------------
| Encode a String according to `RFC3896`.
|
| Returns `Nothing` on errors or if `text` is `Nothing`.
|
| * `text` - The URI String to encode.
-}
encodeURIStringMaybe :: Maybe String -> Maybe String
encodeURIStringMaybe uri = encodeURIComponent =<< uri

{-------------------------------------------------------------------------------
| Return an empty String `""` if the parameter is `Nothing`, the string else.
|
| * s :: Maybe String - if this is `Nothing`, the empty `String` is returned.
|                       Else the string is returned.
-}
showM :: Maybe String -> String
showM = show <<< fromMaybe ""

{-------------------------------------------------------------------------------
| Convert the given object to JSON, as a String.
|
| * `obj` - The object to encode to JSON.
-}
encodeToJsonString ::
  forall a.
  EncodeJson a =>
  a -> String
encodeToJsonString = stringify <<< encodeJson

{-------------------------------------------------------------------------------
| Convert the given JSON as string to an object.
|
| * `str` - The JSON as string to convert to an object.
-}
decodeJsonFromString ::
  forall a.
  DecodeJson a =>
  String -> Either String a
decodeJsonFromString str =
  let
    parsed = jsonParser str
  in
    case parsed of
      Left e -> Left e
      Right j -> case decodeJson j of
        Left er -> Left $ "JSON decode error: " <> printJsonDecodeError er <> stringify j
        Right a -> Right a
