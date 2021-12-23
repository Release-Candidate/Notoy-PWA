-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     Note.purs
-- Date:     23.Dec.2021
--
-- ==============================================================================
-- | Module Data.Note, module holding functions and records about the note data.
module Data.Note
  ( Note(..)
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))

{-
| The actual data and text of the note.
|
| Any of these may be missing (`Nothing`).
| * `title` - the title of the note.
| * `url` - the URL of the website the note is about.
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
