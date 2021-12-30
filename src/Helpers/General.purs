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
  ( decodeURIString
  , decodeURIStringMaybe
  , decodeURLString
  , decodeURLStringMaybe
  , encodeURIString
  , encodeURIStringMaybe
  , encodeURLString
  , encodeURLStringMaybe
  , getFirstMatch
  , getURL
  , regexURL
  , showM
  ) where

import Prelude
import Data.Array.NonEmpty (take)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Regex (Regex, match, test)
import Data.String.Regex.Flags (ignoreCase, unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import JSURI (decodeFormURLComponent, decodeURIComponent, encodeFormURLComponent, encodeURIComponent)

{-------------------------------------------------------------------------------
| Regex to parse an URL.
-}
regexURL :: Regex
--regexURL = unsafeRegex "((ht|f)tps?|chrome):\\/\\/[^\\s$.?#].[^\\s]*" unicode
regexURL = unsafeRegex "(?:(?:https?|ftp):\\/\\/)(?:\\S+(?::\\S*)?@)?(?:(?!(?:10|127)(?:\\.\\d{1,3}){3})(?!(?:169\\.254|192\\.168)(?:\\.\\d{1,3}){2})(?!172\\.(?:1[6-9]|2\\d|3[0-1])(?:\\.\\d{1,3}){2})(?:[1-9]\\d?|1\\d\\d|2[01]\\d|22[0-3])(?:\\.(?:1?\\d{1,2}|2[0-4]\\d|25[0-5])){2}(?:\\.(?:[1-9]\\d?|1\\d\\d|2[0-4]\\d|25[0-4]))|(?:(?:[a-z\\u00a1-\\uffff0-9]-*)*[a-z\\u00a1-\\uffff0-9]+)(?:\\.(?:[a-z\\u00a1-\\uffff0-9]-*)*[a-z\\u00a1-\\uffff0-9]+)*(?:\\.(?:[a-z\\u00a1-\\uffff]{2,}))\\.?)(?::\\d{2,5})?(?:[/?#]\\S*)?" ignoreCase

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
| Search for an URL in the given String and return that.
|
| If no URL has been found, `Nothing` is returned.
|
| * `text` - The String to parse for an URL.
-}
getURL :: String -> Maybe String
getURL text = case test regexURL text of
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
decodeURLString text = case decodeFormURLComponent text of
  Nothing -> ""
  Just txt -> txt

{-------------------------------------------------------------------------------
| Decode a String according to `RFC3896`.
|
| Returns the empty String `""` on errors.
|
| * `text` - The URI-encoded String to decode.
-}
decodeURIString :: String -> String
decodeURIString text = case decodeURIComponent text of
  Nothing -> ""
  Just txt -> txt

{-------------------------------------------------------------------------------
| Encode a String accoring to `application/x-www-form-urlencoded`.
|
|  Returns the empty String `""` on errors.
|
| * `text` - The URL to encode.
-}
encodeURLString :: String -> String
encodeURLString uri = case encodeFormURLComponent uri of
  Nothing -> ""
  Just txt -> txt

{-------------------------------------------------------------------------------
| Encode a String accoring to `RFC3896`.
|
|  Returns the empty String `""` on errors.
|
| * `text` - The URL to encode.
-}
encodeURIString :: String -> String
encodeURIString uri = case encodeURIComponent uri of
  Nothing -> ""
  Just txt -> txt

{-------------------------------------------------------------------------------
| Decode a String according to `application/x-www-form-urlencoded`.
|
| Returns `Nothing` on errors or if `text` is `Nothing`.
|
| * `text` - The URL-encoded String to decode.
-}
decodeURLStringMaybe :: Maybe String -> Maybe String
decodeURLStringMaybe text = do
  justTxt <- text
  decodeFormURLComponent justTxt

{-------------------------------------------------------------------------------
| Decode a String according to `RFC3896`.
|
| Returns `Nothing` on errors or if `text` is `Nothing`.
|
| * `text` - The URI-encoded String to decode.
-}
decodeURIStringMaybe :: Maybe String -> Maybe String
decodeURIStringMaybe text = do
  justTxt <- text
  decodeURIComponent justTxt

{-------------------------------------------------------------------------------
| Encode a String according to `application/x-www-form-urlencoded`.
|
| Returns `Nothing` on errors or if `text` is `Nothing`.
|
| * `text` - The URL-encoded String to decode.
-}
encodeURLStringMaybe :: Maybe String -> Maybe String
encodeURLStringMaybe text = do
  justTxt <- text
  encodeFormURLComponent justTxt

{-------------------------------------------------------------------------------
| Encode a String according to `RFC3896`.
|
| Returns `Nothing` on errors or if `text` is `Nothing`.
|
| * `text` - The URI String to encode.
-}
encodeURIStringMaybe :: Maybe String -> Maybe String
encodeURIStringMaybe uri = do
  justUri <- uri
  encodeURIComponent justUri

{-------------------------------------------------------------------------------
| Return an empty String `""` if the parameter is `Nothing`, the string else.
|
| * s :: Maybe String - if this is `Nothing`, the empty `String` is returned.
|                       Else the string is returned.
-}
showM :: Maybe String -> String
showM = show <<< fromMaybe ""
