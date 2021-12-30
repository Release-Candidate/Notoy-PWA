-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     Options.purs
-- Date:     23.Dec.2021
--
-- =============================================================================
-- | Module Data.Options, the record holding the app's options and related
-- | functions.
module Data.Options
  ( Format(..)
  , Options(..)
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

{-------------------------------------------------------------------------------
| The format of the note.
|
| One of
|   * `Markdown` - Markdown formatted text.
|   * `OrgMode` - Emacs Org-Mode formatted text.
|   * `Text` - plain text.
-}
data Format
  = Markdown
  | OrgMode
  | Text

derive instance eqFormat :: Eq Format

derive instance ordFormat :: Ord Format

derive instance genericFormat :: Generic Format _

instance showFormat :: Show Format where
  show = genericShow

{-------------------------------------------------------------------------------
| The apps options.
|
| * `format` - the note format to use. Markdown, Emacs Org-Mode or plain text.
| * `addDate` - add the current date to the note?
| * `addYaml` - add a YAML front matter header to the note?
-}
data Options
  = Options
    { format :: Format
    , addDate :: Boolean
    , addYaml :: Boolean
    }

derive instance eqOptions :: Eq Options

derive instance ordOptions :: Ord Options

derive instance genericOptions :: Generic Options _

instance showOptions :: Show Options where
  show = genericShow
