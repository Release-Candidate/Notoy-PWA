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
import Data.Array (foldr, head, tail)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Note (KeyWordArray(..), Note(..))
import Data.Options (AddDate(..), AddYamlHeader(..), Format(..), Options(..))
import Data.String.Regex (Regex, replace)
import Data.String.Regex.Flags (unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.URL (noteUrlToString)

{-------------------------------------------------------------------------------
| Return a formatted note depending on the options set in `options`.
|
| * `options` - The `Options` to use to format the note.
| * `note` - The `Note` to format.
| * `timestamp` - The timestamp to add to the note if the option is set.
| * `language` - The language this note is written in, used for the YAML front
|                matter header (if enabled in the options).
-}
noteContentString :: Options -> Note -> String -> String -> String
noteContentString options@(Options { format: Markdown }) = noteContentMarkdown options

noteContentString options@(Options { format: OrgMode }) = noteContentOrgMode options

noteContentString options@(Options { format: Text }) = noteContentText options

{-------------------------------------------------------------------------------
| Return a markdown formatted note.
|
| * `options` - The `Options` to use to format the note.
| * `note` - The `Note` to format.
| * `timestamp` - The timestamp to add to the note if the option is set.
| * `language` - The language this note is written in, used for the YAML front
|                matter header (if enabled in the options).
-}
noteContentMarkdown :: Options -> Note -> String -> String -> String
noteContentMarkdown options note timestamp language =
  yamlStrFromNote note options language
    <> "# "
    <> titleStrFromNote note
    <> "\n\n"
    <> keywordStr note options
    <> dateStrFromOptions options timestamp
    <> locationStr note options
    <> shortDescStr note
    <> urlStr note options
    <> longDescStr note

{-------------------------------------------------------------------------------
| Return an Org-Mode formatted note.
|
| * `options` - The `Options` to use to format the note.
| * `note` - The `Note` to format.
| * `timestamp` - The timestamp to add to the note if the option is set.
| * `language` - The language this note is written in. Not used, as Org-Mode
|                has it's own header.
-}
noteContentOrgMode :: Options -> Note -> String -> String -> String
noteContentOrgMode options note timestamp _ =
  "#+title: "
    <> titleStrFromNote note
    <> "\n#+date: "
    <> timestamp
    <> "\n#+FILETAGS"
    <> "\n\n"
    <> "* "
    <> titleStrFromNote note
    <> "\t\t"
    <> keywordsToOrgMode keywords
    <> "\n\n"
    <> keywordStr note options
    <> dateStrFromOptions options timestamp
    <> locationStr note options
    <> shortDescStr note
    <> urlStr note options
    <> longDescStr note
  where
  Note n = note

  keywords = fromMaybe (KeyWordArray []) n.keywords

{-------------------------------------------------------------------------------
| Return a plain text formatted note.
|
| * `options` - The `Options` to use to format the note.
| * `note` - The `Note` to format.
| * `timestamp` - The timestamp to add to the note if the option is set.
| * `language` - The language this note is written in, used for the YAML front
|                matter header (if enabled in the options).
-}
noteContentText :: Options -> Note -> String -> String -> String
noteContentText options note timestamp language =
  yamlStrFromNote note options language
    <> titleStrFromNote note
    <> "\n\n"
    <> keywordStr note options
    <> dateStrFromOptions options timestamp
    <> locationStr note options
    <> shortDescStr note
    <> urlStr note options
    <> longDescStr note

{-------------------------------------------------------------------------------
| Return the YAML front matter header as a string.
-}
yamlStrFromNote :: Note -> Options -> String -> String
yamlStrFromNote (Note note) (Options { addYaml: AddYamlHeader }) language =
  "---\ntitle: "
    <> fromMaybe "" note.title
    <> "\nauthor:\n  -\nkeywords:"
    <> keywordsToYaml keywords
    <> "\ntags:"
    <> keywordsToYaml keywords
    <> "\nlang: "
    <> language
    <> "\n---\n\n"
  where
  keywords = fromMaybe (KeyWordArray []) note.keywords

yamlStrFromNote _ (Options { addYaml: NoYamlHeader }) _ = ""

{-------------------------------------------------------------------------------
| Return the title of the note as a string.
|
| If the title is `Nothing`, "Title" is returned.
-}
titleStrFromNote :: Note -> String
titleStrFromNote (Note { title: titleM }) = (fromMaybe "Title" titleM)

{-------------------------------------------------------------------------------
| Return the keywords as a comma separated list of tags with a hash sign `#` as
| prefix of each keyword and all whitespace in the keywords replaced by one
| underscore `_`.
-}
keywordStr :: Note -> Options -> String
keywordStr (Note { keywords: Nothing }) _ = ""

keywordStr (Note { keywords: Just keywordArr }) (Options { format: Markdown }) =
  "Keywords: "
    <> keywordsAddHash keywordArr
    <> "\n"

keywordStr (Note { keywords: Just keywordArr }) (Options { format: OrgMode }) =
  "Keywords: "
    <> keywordsAddHash keywordArr
    <> "\n"

keywordStr (Note { keywords: Just keywordArr }) (Options { format: Text }) =
  "Keywords: "
    <> keywordsAddHash keywordArr
    <> "\n"

{-------------------------------------------------------------------------------
| Return the date timestamp as a string, suitable for the note format used.
|
| If the format is `OrgMode`, the date is returned in angle brackets `<DATE>`.
-}
dateStrFromOptions :: Options -> String -> String
dateStrFromOptions (Options { addDate: AddDate, format: OrgMode }) timestamp =
  "<"
    <> timestamp
    <> "> "

dateStrFromOptions (Options { addDate: AddDate, format: _ }) timestamp = timestamp <> " "

dateStrFromOptions (Options { addDate: NoDate }) _ = ""

{-------------------------------------------------------------------------------
| Return the device's position as a string.
-}
locationStr :: Note -> Options -> String
locationStr (Note { location: Nothing }) _ = "\n\n"

locationStr (Note { location: Just loc }) (Options { format: Markdown }) = loc <> "\n\n"

locationStr (Note { location: Just loc }) (Options { format: OrgMode }) = loc <> "\n\n"

locationStr (Note { location: Just loc }) (Options { format: Text }) = loc <> "\n\n"

{-------------------------------------------------------------------------------
| Return the short description of the note as a string.
-}
shortDescStr :: Note -> String
shortDescStr (Note { shortDesc: Just shortDesc }) = shortDesc <> "\n"

shortDescStr (Note { shortDesc: Nothing }) = ""

{-------------------------------------------------------------------------------
| Return the URL of the note as a string, depending on the format of the note.
-}
urlStr ∷ Note → Options → String
urlStr (Note { url: Nothing }) _ = ""

urlStr note@(Note { url: Just url }) (Options { format: Markdown }) =
  "["
    <> title
    <> "]("
    <> urlSt
    <> ")\n"
  where
  urlSt = noteUrlToString url

  Note n = note

  title = case n.title of
    Nothing -> urlSt
    Just t -> t

urlStr note@(Note { url: Just url }) (Options { format: OrgMode }) =
  "[["
    <> urlSt
    <> "]["
    <> title
    <> "]]\n"
  where
  urlSt = noteUrlToString url

  Note n = note

  title = case n.title of
    Nothing -> urlSt
    Just t -> t

urlStr (Note { url: Just url }) (Options { format: Text }) = urlSt <> "\n"
  where
  urlSt = noteUrlToString url

{-------------------------------------------------------------------------------
| Return the detailed description of the note as a string.
-}
longDescStr :: Note -> String
longDescStr (Note { longDesc: Nothing }) = ""

longDescStr (Note { longDesc: Just longDesc }) = longDesc <> "\n"

{-------------------------------------------------------------------------------
| Return the keywords as Org-Mode formatted keywords, that is, as double colon
| `:` separated list of keywords, where whitespace has been replaced by an
| underscore `_`.
-}
keywordsToOrgMode :: KeyWordArray -> String
keywordsToOrgMode (KeyWordArray keywords) =
  foldr (\e acc -> ":" <> e <> acc) ":"
    $ whitespaceToUnderscore keywords

{-------------------------------------------------------------------------------
| Return the keywords as a YAML list, with whitespace replaced by an underscore
| `_`.
-}
keywordsToYaml :: KeyWordArray -> String
keywordsToYaml (KeyWordArray keywords) =
  foldr (\e acc -> "\n  - " <> e <> acc) ""
    $ whitespaceToUnderscore keywords

{-------------------------------------------------------------------------------
| Return the keywords as a comma separated list of tags with a hash sign `#` as
| prefix of each keyword and all whitespace in the keywords replaced by one
| underscore `_`.
-}
keywordsAddHash :: KeyWordArray -> String
keywordsAddHash (KeyWordArray keywords) =
  "#"
    <> frst
    <> foldr (\e acc -> ", #" <> e <> acc) "" rest
  where
  sanitized = whitespaceToUnderscore keywords

  frst = fromMaybe "" $ head sanitized

  rest = fromMaybe [] $ tail sanitized

{-------------------------------------------------------------------------------
| Replace all whitespace in the string with an underscore `_`.
-}
whitespaceToUnderscore :: Array String -> Array String
whitespaceToUnderscore = map (\e -> replace whitespaceToUnderscoreRex "_" e)

{-------------------------------------------------------------------------------
| Regex to match consecutive whitespace.
-}
whitespaceToUnderscoreRex :: Regex
whitespaceToUnderscoreRex = unsafeRegex "\\s+" unicode
