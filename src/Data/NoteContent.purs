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
import Data.Array (foldr)
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
|                matter header.
-}
noteContentString :: Options -> Note -> String -> String -> String
noteContentString options@(Options { format: Markdown }) = noteContentMarkdown options

noteContentString options@(Options { format: OrgMode }) = noteContentOrgMode options

noteContentString options@(Options { format: Text }) = noteContentText options

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

titleStrFromNote :: Note -> String
titleStrFromNote (Note { title: titleM }) = (fromMaybe "Title" titleM)

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

dateStrFromOptions :: Options -> String -> String
dateStrFromOptions (Options { addDate: AddDate, format: OrgMode }) timestamp =
  "<"
    <> timestamp
    <> "> "

dateStrFromOptions (Options { addDate: AddDate, format: _ }) timestamp = timestamp <> " "

dateStrFromOptions (Options { addDate: NoDate }) _ = ""

locationStr :: Note -> Options -> String
locationStr (Note { location: Nothing }) _ = "\n\n"

locationStr (Note { location: Just loc }) (Options { format: Markdown }) = loc <> "\n\n"

locationStr (Note { location: Just loc }) (Options { format: OrgMode }) = loc <> "\n\n"

locationStr (Note { location: Just loc }) (Options { format: Text }) = loc <> "\n\n"

shortDescStr :: Note -> String
shortDescStr (Note { shortDesc: Just shortDesc }) = shortDesc <> "\n"

shortDescStr (Note { shortDesc: Nothing }) = ""

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

longDescStr :: Note -> String
longDescStr (Note { longDesc: Nothing }) = ""

longDescStr (Note { longDesc: Just longDesc }) = longDesc <> "\n"

keywordsToOrgMode :: KeyWordArray -> String
keywordsToOrgMode (KeyWordArray keywords) =
  foldr (\e acc -> ":" <> e <> acc) ":"
    $ whitespaceToUnderscore keywords

keywordsToYaml :: KeyWordArray -> String
keywordsToYaml (KeyWordArray keywords) =
  foldr (\e acc -> "\n  - " <> e <> acc) ""
    $ whitespaceToUnderscore keywords

keywordsAddHash :: KeyWordArray -> String
keywordsAddHash (KeyWordArray keywords) =
  replace firstComma ""
    $ foldr (\e acc -> ", #" <> e <> acc) ""
    $ whitespaceToUnderscore keywords

whitespaceToUnderscore :: Array String -> Array String
whitespaceToUnderscore = map (\e -> replace whitespaceToUnderscoreRex "_" e)

whitespaceToUnderscoreRex :: Regex
whitespaceToUnderscoreRex = unsafeRegex "\\s+" unicode

firstComma :: Regex
firstComma = unsafeRegex ",\\s*" unicode
