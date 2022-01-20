-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2022 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     DateTimeFormat.purs
-- Date:     20.Jan.2022
--
-- ==============================================================================
-- | Module Data.DateTimeFormat, to set the locale and format of a date or time.
module Data.DateTimeFormat
  ( DateStyle(..)
  , DateTimeFormat
  , Locale(..)
  , TimeStyle(..)
  , TimeZoneNameStyle(..)
  , defaultDateTimeFormat
  , formatDateTimeNow
  , localeToString
  , stringToLocale
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Test.QuickCheck (class Arbitrary, arbitrary)

{-------------------------------------------------------------------------------
| Type to hold the locale and formatting options of a date or a time.
-}
foreign import data DateTimeFormat :: Type

{-------------------------------------------------------------------------------
| Return the default `DateTimeFormat` object of the current (browser) locale.
-}
foreign import defaultDateTimeFormat :: Unit -> Effect DateTimeFormat

{-------------------------------------------------------------------------------
| Return a string of the current date and time formatted using the given
| formatter object.
-}
foreign import formatDateTimeNow :: DateTimeFormat -> Effect String

{-------------------------------------------------------------------------------
| The style with which to format the date.
|
| One of
|  * Full
|  * Long
|  * Medium
|  * Short
-}
data DateStyle
  = Full
  | Long
  | Medium
  | Short

{-------------------------------------------------------------------------------
| The possible values of the `dateStyle` field in the Javascript options record.
-}
dateStyleJS ::
  { full :: String
  , long :: String
  , medium :: String
  , short :: String
  }
dateStyleJS =
  { full: "full"
  , long: "long"
  , medium: "medium"
  , short: "short"
  }

toDateStyleJS :: DateStyle -> String
toDateStyleJS Full = dateStyleJS.full

toDateStyleJS Long = dateStyleJS.long

toDateStyleJS Medium = dateStyleJS.medium

toDateStyleJS Short = dateStyleJS.short

derive instance eqDateStyle :: Eq DateStyle

derive instance ordDateStyle :: Ord DateStyle

derive instance genericDateStyle :: Generic DateStyle _

instance decodeJsonDateStyle :: DecodeJson DateStyle where
  decodeJson = genericDecodeJson

instance encodeJsonDateStyle :: EncodeJson DateStyle where
  encodeJson = genericEncodeJson

instance showDateStyle :: Show DateStyle where
  show = genericShow

{-------------------------------------------------------------------------------
| ATTENTION: 4 is the number of values of `DateStyle`.
-}
instance arbitraryDateStyle :: Arbitrary DateStyle where
  arbitrary = map intToDateStyle arbitrary
    where
    intToDateStyle :: Int -> DateStyle
    intToDateStyle n
      | n >= 0 = case n `mod` 4 of
        0 -> Full
        1 -> Long
        2 -> Medium
        _ -> Short
      | otherwise = intToDateStyle (-n)

{-------------------------------------------------------------------------------
| The style of the time format.
|
| One of
|   * FullT
|   * LongT
|   * MediumT
|   * ShortT
-}
data TimeStyle
  = FullT
  | LongT
  | MediumT
  | ShortT

timeStyleJS ::
  { full :: String
  , long :: String
  , medium :: String
  , short :: String
  }
timeStyleJS =
  { full: "full"
  , long: "long"
  , medium: "medium"
  , short: "short"
  }

{-------------------------------------------------------------------------------
| The possible values of the `timeStyle` field in the Javascript options record.
-}
toTimeStyleJS :: TimeStyle -> String
toTimeStyleJS FullT = timeStyleJS.full

toTimeStyleJS LongT = timeStyleJS.long

toTimeStyleJS MediumT = timeStyleJS.medium

toTimeStyleJS ShortT = timeStyleJS.short

derive instance eqTimeStyle :: Eq TimeStyle

derive instance ordTimeStyle :: Ord TimeStyle

derive instance genericTimeStyle :: Generic TimeStyle _

instance decodeJsonTimeStyle :: DecodeJson TimeStyle where
  decodeJson = genericDecodeJson

instance encodeJsonTimeStyle :: EncodeJson TimeStyle where
  encodeJson = genericEncodeJson

instance showTimeStyle :: Show TimeStyle where
  show = genericShow

{-------------------------------------------------------------------------------
| ATTENTION: 4 is the number of values of `TimeStyle`.
-}
instance arbitraryTimeStyle :: Arbitrary TimeStyle where
  arbitrary = map intToTimeStyle arbitrary
    where
    intToTimeStyle :: Int -> TimeStyle
    intToTimeStyle n
      | n >= 0 = case n `mod` 4 of
        0 -> FullT
        1 -> LongT
        2 -> MediumT
        _ -> ShortT
      | otherwise = intToTimeStyle (-n)

{-------------------------------------------------------------------------------
| The style of the time zone name.
|
| One of
|   * Short
|   * Long
|   * ShortOffset
|   * LongOffset
|   * ShortGeneric
|   * LongGeneric
-}
data TimeZoneNameStyle
  = ShortTZ
  | LongTZ
  | ShortOffsetTZ
  | LongOffsetTZ
  | ShortGenericTZ
  | LongGenericTZ

{-------------------------------------------------------------------------------
| The possible values of the `timeZoneName` field in the Javascript options
| record.
-}
timeZoneNameStyleJS ::
  { long :: String
  , longGeneric :: String
  , longOffset :: String
  , short :: String
  , shortGeneric :: String
  , shortOffset :: String
  }
timeZoneNameStyleJS =
  { short: "short"
  , long: "long"
  , shortOffset: "shortOffset"
  , longOffset: "longOffset"
  , shortGeneric: "shortGeneric"
  , longGeneric: "longGeneric"
  }

toTimeZoneNameStyle :: TimeZoneNameStyle -> String
toTimeZoneNameStyle ShortTZ = timeZoneNameStyleJS.short

toTimeZoneNameStyle LongTZ = timeZoneNameStyleJS.long

toTimeZoneNameStyle ShortOffsetTZ = timeZoneNameStyleJS.shortOffset

toTimeZoneNameStyle LongOffsetTZ = timeZoneNameStyleJS.longOffset

toTimeZoneNameStyle ShortGenericTZ = timeZoneNameStyleJS.shortGeneric

toTimeZoneNameStyle LongGenericTZ = timeZoneNameStyleJS.longGeneric

derive instance eqTimeZoneNameStyle :: Eq TimeZoneNameStyle

derive instance ordTimeZoneNameStyle :: Ord TimeZoneNameStyle

derive instance genericTimeZoneNameStyle :: Generic TimeZoneNameStyle _

instance decodeJsonTimeZoneNameStyle :: DecodeJson TimeZoneNameStyle where
  decodeJson = genericDecodeJson

instance encodeJsonTimeZoneNameStyle :: EncodeJson TimeZoneNameStyle where
  encodeJson = genericEncodeJson

instance showTimeZoneNameStyle :: Show TimeZoneNameStyle where
  show = genericShow

{-------------------------------------------------------------------------------
| ATTENTION: 6 is the number of values of `TimeZoneNameStyle`.
-}
instance arbitraryTimeZoneNameStyle :: Arbitrary TimeZoneNameStyle where
  arbitrary = map intToTimeZoneNameStyle arbitrary
    where
    intToTimeZoneNameStyle :: Int -> TimeZoneNameStyle
    intToTimeZoneNameStyle n
      | n >= 0 = case n `mod` 6 of
        0 -> ShortTZ
        1 -> LongTZ
        2 -> ShortOffsetTZ
        3 -> LongOffsetTZ
        4 -> ShortGenericTZ
        _ -> LongGenericTZ
      | otherwise = intToTimeZoneNameStyle (-n)

{-------------------------------------------------------------------------------
| The type of a locale.
|
| This is a wrapped BCP 47 language tag.
-}
newtype Locale
  = Locale String

derive newtype instance eqLocale :: Eq Locale

derive newtype instance ordLocale :: Ord Locale

derive newtype instance showLocale :: Show Locale

derive newtype instance decodeJsonLocale :: DecodeJson Locale

derive newtype instance encodeJsonLocale :: EncodeJson Locale

derive instance genericLocale :: Generic Locale _

derive instance newtypeLocale :: Newtype Locale _

derive newtype instance arbitraryLocale :: Arbitrary Locale

{-------------------------------------------------------------------------------
| Convert a `Locale` to a `String`.
|
| * `locale` - The `Locale` to convert to a `String`.
-}
localeToString :: Locale -> String
localeToString = unwrap

{-------------------------------------------------------------------------------
| Convert a BCP 47 language tag String to a `Locale`.
|
| The given String is not validated, every String returns a Locale!
|
| * `str` - The `String` to convert to a `Locale`.
-}
stringToLocale :: String -> Locale
stringToLocale = wrap
