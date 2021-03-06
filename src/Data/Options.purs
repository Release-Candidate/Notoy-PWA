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
  ( AddDate(..)
  , AddYamlHeader(..)
  , Format(..)
  , LookupLocation(..)
  , Options(..)
  , addDateFromBool
  , defaultOptions
  , formatFromString
  , lookupLocationFromBool
  , noteFileMime
  , noteFileSuffix
  , optionsKeyId
  , yamlHeaderFromBool
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.StoreKey (class StoreKey, StoreKeyId(..))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)

{-------------------------------------------------------------------------------
| The `StoreKeyId` of `Options`.
-}
optionsKeyId :: StoreKeyId
optionsKeyId = StoreKeyId "Options"

{-------------------------------------------------------------------------------
| The apps options.
|
| * `format` - the note format to use. Markdown, Emacs Org-Mode or plain text.
| * `addDate` - add the current date to the note?
| * `lookupLocation` - do reverse geolocation?
| * `addYaml` - add a YAML front matter header to the note?
-}
newtype Options
  = Options
  { format :: Format
  , addDate :: AddDate
  , lookupLocation :: LookupLocation
  , addYaml :: AddYamlHeader
  }

{-------------------------------------------------------------------------------
| The default `Options`, markdown format with the current date as timestamp,
| but no YAML front matter or reverse geolocation of the position.
-}
defaultOptions ∷ Options
defaultOptions =
  Options
    { format: Markdown
    , addDate: AddDate
    , lookupLocation: NoReverseGeolocation
    , addYaml: NoYamlHeader
    }

derive instance eqOptions :: Eq Options

derive instance ordOptions :: Ord Options

derive instance genericOptions :: Generic Options _

instance decodeJsonOptions :: DecodeJson Options where
  decodeJson = genericDecodeJson

instance encodeJsonOptions :: EncodeJson Options where
  encodeJson = genericEncodeJson

instance showOptions :: Show Options where
  show = genericShow

instance arbitraryOptions :: Arbitrary Options where
  arbitrary = genericArbitrary

instance storeKeyIdOptions :: StoreKey Options where
  key _ = optionsKeyId

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

instance decodeJsonFormat :: DecodeJson Format where
  decodeJson = genericDecodeJson

instance encodeJsonFormat :: EncodeJson Format where
  encodeJson = genericEncodeJson

instance showFormat :: Show Format where
  show = genericShow

{-------------------------------------------------------------------------------
 ATTENTION: 3 is the number of values of `Format`.
-}
instance arbitraryFormat :: Arbitrary Format where
  arbitrary = map intToFormat arbitrary
    where
    intToFormat :: Int -> Format
    intToFormat n
      | n >= 0 = case n `mod` 3 of
        0 -> Markdown
        1 -> OrgMode
        _ -> Text
      | otherwise = intToFormat (-n)

{-------------------------------------------------------------------------------
| Whether to automatically add the current date as timestamp to the note.
|
| One of:
|   * NoDate
|   * AddDate
-}
data AddDate
  = NoDate
  | AddDate

derive instance eqAddDate :: Eq AddDate

derive instance ordAddDate :: Ord AddDate

derive instance genericAddDate :: Generic AddDate _

instance decodeJsonAddDate :: DecodeJson AddDate where
  decodeJson = genericDecodeJson

instance encodeJsonAddDate :: EncodeJson AddDate where
  encodeJson = genericEncodeJson

instance showAddDate :: Show AddDate where
  show = genericShow

{-------------------------------------------------------------------------------
 ATTENTION: 2 is the number of values of `AddDate`!
-}
instance arbitraryAddDate :: Arbitrary AddDate where
  arbitrary = map intToAddDate arbitrary
    where
    intToAddDate :: Int -> AddDate
    intToAddDate n
      | n >= 0 = case n `mod` 2 of
        0 -> NoDate
        _ -> AddDate
      | otherwise = intToAddDate (-n)

{-------------------------------------------------------------------------------
| Whether to do a reverse geolocation lookup of the position or not.
|
| One of
|   * ReverseGeolocation
|   * NoReverseGeolocation
-}
data LookupLocation
  = ReverseGeolocation
  | NoReverseGeolocation

derive instance eqLookupLocation :: Eq LookupLocation

derive instance ordLookupLocation :: Ord LookupLocation

derive instance genericLookupLocation :: Generic LookupLocation _

instance decodeJsonLookupLocation :: DecodeJson LookupLocation where
  decodeJson = genericDecodeJson

instance encodeJsonLookupLocation :: EncodeJson LookupLocation where
  encodeJson = genericEncodeJson

instance showLookupLocation :: Show LookupLocation where
  show = genericShow

{-------------------------------------------------------------------------------
 ATTENTION: 2 is the number of values of `LookupLocation`.
-}
instance arbitraryLookupLocation :: Arbitrary LookupLocation where
  arbitrary = map intToLookupLocation arbitrary
    where
    intToLookupLocation :: Int -> LookupLocation
    intToLookupLocation n
      | n >= 0 = case n `mod` 2 of
        0 -> ReverseGeolocation
        _ -> NoReverseGeolocation
      | otherwise = intToLookupLocation (-n)

{-------------------------------------------------------------------------------
| Whether to automatically add a YAML front matter header to the note.
|
| One of:
|   * NoYamlHeader
|   * AddYamlHeader
-}
data AddYamlHeader
  = NoYamlHeader
  | AddYamlHeader

derive instance eqAddYamlHeader :: Eq AddYamlHeader

derive instance ordAddYamlHeader :: Ord AddYamlHeader

derive instance genericAddYamlHeader :: Generic AddYamlHeader _

instance decodeJsonAddYamlHeader :: DecodeJson AddYamlHeader where
  decodeJson = genericDecodeJson

instance encodeJsonAddYamlHeader :: EncodeJson AddYamlHeader where
  encodeJson = genericEncodeJson

instance showAddYamlHeader :: Show AddYamlHeader where
  show = genericShow

{-------------------------------------------------------------------------------
 ATTENTION: 2 is the number of values of `AddYamlHeader`!
-}
instance arbitraryAddYamlHeader :: Arbitrary AddYamlHeader where
  arbitrary = map intToAddYamlHeader arbitrary
    where
    intToAddYamlHeader :: Int -> AddYamlHeader
    intToAddYamlHeader n
      | n >= 0 = case n `mod` 2 of
        0 -> NoYamlHeader
        _ -> AddYamlHeader
      | otherwise = intToAddYamlHeader (-n)

{-------------------------------------------------------------------------------
| Convert a `String` to a `Format`.
|
| If the given string is not one of the `Format` options, `Text` is returned.
|
| * `st` - The string representation of the `Format` to convert to a `Format`.
-}
formatFromString :: String -> Format
formatFromString st
  | st == show Markdown = Markdown
  | st == show OrgMode = OrgMode
  | otherwise = Text

{-------------------------------------------------------------------------------
| Convert a `Boolean` to an `AddDate`.
|
| * `b` - If `b` is `true`, return `AddDate`. Else return `NoDate`.
-}
addDateFromBool :: Boolean -> AddDate
addDateFromBool false = NoDate

addDateFromBool true = AddDate

{-------------------------------------------------------------------------------
| Convert a `Boolean` to an `LookupLocation`.
|
| * `b` - If `b` is `true`, return `ReverseGeolocation`. Else return
|         `NoReverseGeolocation`.
-}
lookupLocationFromBool :: Boolean -> LookupLocation
lookupLocationFromBool false = NoReverseGeolocation

lookupLocationFromBool true = ReverseGeolocation

{-------------------------------------------------------------------------------
| Convert a `Boolean` to an `AddYamlHeader`.
|
| * `b` - If `b` is `true`, return `AddYamlHeader`. Else return `NoYamlHeader`.
-}
yamlHeaderFromBool :: Boolean -> AddYamlHeader
yamlHeaderFromBool false = NoYamlHeader

yamlHeaderFromBool true = AddYamlHeader

{-------------------------------------------------------------------------------
| Return the file suffix for the given `Format`.
|
| - Markdown: `.md`
| - OrgMode: `.org`
| - Text: `.txt`
|
| * `options` - The `Options` object with the format to use.
-}
noteFileSuffix :: Options -> String
noteFileSuffix (Options { format: Markdown }) = fileSuffix.markdown

noteFileSuffix (Options { format: OrgMode }) = fileSuffix.orgMode

noteFileSuffix (Options { format: Text }) = fileSuffix.text

{-------------------------------------------------------------------------------
| Return the MIME type for the given `Format`.
|
| - Markdown: "text/markdown"
| - OrgMode: "text/org"
| - Text: "text/plain"
|
| * `options` - The `Options` object with the format to use.
-}
noteFileMime :: Options -> String
noteFileMime (Options { format: Markdown }) = mimeTypes.markdown

noteFileMime (Options { format: OrgMode }) = mimeTypes.orgMode

noteFileMime (Options { format: Text }) = mimeTypes.text

{-------------------------------------------------------------------------------
| File suffixes of the document formats.
-}
fileSuffix ::
  { markdown :: String
  , orgMode :: String
  , text :: String
  }
fileSuffix =
  { markdown: ".md"
  , orgMode: ".org"
  , text: ".txt"
  }

{-------------------------------------------------------------------------------
| MIME type of the various document formats.
-}
mimeTypes ::
  { markdown :: String
  , orgMode :: String
  , text :: String
  }
mimeTypes =
  { markdown: "text/markdown"
  , orgMode: "text/org"
  , text: "text/plain"
  }
