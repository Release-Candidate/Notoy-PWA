-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2022 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     NoteContent.purs
-- Date:     17.Jan.2022
--
-- ==============================================================================
-- | Module Data.NoteContent, contains everything to generate a formatted note
-- | from `Note` and `Options` instances.
module Data.NoteContent
  ( noteContentString
  ) where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Note (Note(..))
import Data.Options (AddDate(..), AddYamlHeader(..), Format(..), Options(..))

{-------------------------------------------------------------------------------
| Return a formatted note depending on the options set in `options`.
|
| * `options` - The `Options` to use to format the note.
| * `note` - The `Note`  to format.
-}
noteContentString :: Options -> Note -> String
noteContentString options@(Options { format: Markdown }) = noteContentMarkdown options

noteContentString options@(Options { format: OrgMode }) = noteContentOrgMode options

noteContentString options@(Options { format: Text }) = noteContentText options

noteContentMarkdown :: Options -> Note -> String
noteContentMarkdown options note =
  yamlStrFromNote note options
    <> "# "
    <> titleStrFromNote note
    <> keywordStr note options
    <> dateStrFromOptions options
    <> shortDescStr note
    <> urlStr note options
    <> longDescStr note

noteContentOrgMode :: Options -> Note -> String
noteContentOrgMode options note =
  yamlStrFromNote note options
    <> "* "
    <> titleStrFromNote note
    <> keywordStr note options
    <> dateStrFromOptions options
    <> shortDescStr note
    <> urlStr note options
    <> longDescStr note

noteContentText :: Options -> Note -> String
noteContentText options note =
  yamlStrFromNote note options
    <> titleStrFromNote note
    <> keywordStr note options
    <> dateStrFromOptions options
    <> shortDescStr note
    <> urlStr note options
    <> longDescStr note

yamlStrFromNote :: Note -> Options -> String
yamlStrFromNote note (Options { addYaml: AddYamlHeader }) = "\n\n"

yamlStrFromNote note (Options { addYaml: NoYamlHeader }) = ""

titleStrFromNote :: Note -> String
titleStrFromNote (Note { title: titleM }) = (fromMaybe "Title" titleM) <> "\n\n"

keywordStr :: Note -> Options -> String
keywordStr (Note { keywords: Nothing }) _ = ""

keywordStr (Note { keywords: Just keywordArr }) (Options { format: Markdown }) = ""

keywordStr (Note { keywords: Just keywordArr }) (Options { format: OrgMode }) = ""

keywordStr (Note { keywords: Just keywordArr }) (Options { format: Text }) = ""

dateStrFromOptions :: Options -> String
dateStrFromOptions (Options { addDate: AddDate }) = "\n\n"

dateStrFromOptions (Options { addDate: NoDate }) = ""

shortDescStr :: Note -> String
shortDescStr (Note { shortDesc: Just shortDesc }) = shortDesc <> "\n"

shortDescStr (Note { shortDesc: Nothing }) = ""

urlStr ∷ Note → Options → String
urlStr (Note { url: Nothing }) _ = ""

urlStr (Note { url: Just url }) (Options { format: Markdown }) = ""

urlStr (Note { url: Just url }) (Options { format: OrgMode }) = ""

urlStr (Note { url: Just url }) (Options { format: Text }) = ""

longDescStr :: Note -> String
longDescStr (Note { longDesc: Nothing }) = ""

longDescStr (Note { longDesc: Just longDesc }) = longDesc <> "\n"
