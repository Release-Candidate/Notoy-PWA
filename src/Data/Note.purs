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
  ( Note(..)
  , fromShared
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.String (trim)
import Data.Tuple (Tuple(..))
import Helpers.General (getURL)

{-------------------------------------------------------------------------------
| The actual data and text of the note.
|
| Any of these may be missing (`Nothing`).
| * `title` - the title of the note.
| * `url` - the URL of the website the note is about.
|           ATTENTION: has to be encoded to work as a link!
| * `shrtDesc` - the short description text.
| * `longDesc` - the longer, detailed description.
-}
data Note
  = Note
    { title :: Maybe String
    , url :: Maybe String
    , shortDesc :: Maybe String
    , longDesc :: Maybe String
    }

derive instance eqNote :: Eq Note

derive instance ordNote :: Ord Note

derive instance genericNote :: Generic Note _

instance showNote :: Show Note where
  show ( Note
      { title: title
    , url: url
    , shortDesc: shortDesc
    , longDesc: longDesc
    }
  ) =
    let
      showField :: String -> Maybe String -> String
      showField name val = case val of
        Just s -> name <> ": " <> s <> " "
        Nothing -> ""

      titleString = showField "Title" title

      urlString = showField "URL" url

      shortString = showField "Short Description" shortDesc

      longString = showField "Detailed Description" longDesc
    in
      titleString <> urlString <> shortString <> longString

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
fromShared :: Maybe String -> Maybe String -> Maybe String -> Note
fromShared (Just title) Nothing Nothing =
  Note
    { title: txt
    , url: urlSt
    , shortDesc: Nothing
    , longDesc: Nothing
    }
  where
  UrlString (Tuple urlSt txt) = getURLAndText title

fromShared Nothing Nothing (Just text) =
  Note
    { title: Nothing
    , url: urlSt
    , shortDesc: txt
    , longDesc: Nothing
    }
  where
  UrlString (Tuple urlSt txt) = getURLAndText text

fromShared (Just title) Nothing (Just text) =
  Note
    { title: tl
    , url: foundURL
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
    { title: title
    , url: url
    , shortDesc: text
    , longDesc: Nothing
    }

{-------------------------------------------------------------------------------
| Helper: Type to hold a Tuple of the parsed URL and text (URL, text), both as a
| `Maybe String`.
|
| The URL is `Nothing`, if no URL could be found in `text`.
|
| The text is `Nothing`, if it only consisted of the URL (and whitespace).
-}
newtype UrlString
  = UrlString (Tuple (Maybe String) (Maybe String))

{-------------------------------------------------------------------------------
| Helper:
-}
getURLAndText :: String -> UrlString
getURLAndText text = UrlString (Tuple urlSt txt)
  where
  trimmed = Just $ trim text

  urlSt = getURL text

  txt = if trimmed == urlSt then Nothing else trimmed
