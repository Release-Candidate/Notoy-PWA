-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2022 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     CalendarFormat.purs
-- Date:     23.Jan.2022
--
-- ==============================================================================
-- | Module Data.CalendarFormat, the type of the format of a calendar.
module Data.CalendarFormat
  ( CalendarFormat(..)
  , calendarFormatToString
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Test.QuickCheck (class Arbitrary, arbitrary)

{-------------------------------------------------------------------------------
| The calendar format.
|
| The calendar `islamicc` is deprecated, use `islamic-civil` instead.
|
| See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Locale/calendar#unicode_calendar_keys
|
| One of
|    * Buddhist - Thai Buddhist calendar
|    * Chinese - Traditional Chinese calendar
|    * Coptic - Coptic calendar
|    * Dangi - Traditional Korean calendar
|    * Ethioaa - Ethiopic calendar, Amete Alem (epoch approx. 5493 B.C.E)
|    * Ethiopic - Ethiopic calendar, Amete Mihret (epoch approx, 8 C.E.)
|    * Gregory - Gregorian calendar
|    * Hebrew - Traditional Hebrew calendar
|    * Indian - Indian calendar
|    * Islamic - Islamic calendar
|    * IslamicUmalqura - Islamic calendar, Umm al-Qura
|    * IslamicTbla - Islamic calendar, tabular (intercalary years [2,5,7,10,13,16,18,21,24,26,29] - astronomical epoch)
|    * IslamicCivil - Islamic calendar, tabular (intercalary years [2,5,7,10,13,16,18,21,24,26,29] - civil epoch)
|    * IslamicRgsa - Islamic calendar, Saudi Arabia sighting
|    * Iso8601 - ISO calendar (Gregorian calendar using the ISO 8601 calendar week rules)
|    * Japanese - Japanese Imperial calendar
|    * Persian - Persian calendar
|    * Roc - Civil (algorithmic) Arabic calendar
-}
data CalendarFormat
  = Buddhist
  | Chinese
  | Coptic
  | Dangi
  | Ethioaa
  | Ethiopic
  | Gregory
  | Hebrew
  | Indian
  | Islamic
  | IslamicUmalqura
  | IslamicTbla
  | IslamicCivil
  | IslamicRgsa
  | Iso8601
  | Japanese
  | Persian
  | Roc

{-------------------------------------------------------------------------------
| Convert the `CalendarFormat` to a string.
-}
calendarFormatToString :: CalendarFormat -> String
calendarFormatToString Buddhist = "buddhist"

calendarFormatToString Chinese = "chinese"

calendarFormatToString Coptic = "coptic"

calendarFormatToString Dangi = "dangi"

calendarFormatToString Ethioaa = "ethioaa"

calendarFormatToString Ethiopic = "ethiopic"

calendarFormatToString Gregory = "gregory"

calendarFormatToString Hebrew = "hebrew"

calendarFormatToString Indian = "indian"

calendarFormatToString Islamic = "islamic"

calendarFormatToString IslamicUmalqura = "islamic-umalqura"

calendarFormatToString IslamicTbla = "islamic-tbla"

calendarFormatToString IslamicCivil = "islamic-civil"

calendarFormatToString IslamicRgsa = "islamic-rgsa"

calendarFormatToString Iso8601 = "iso8601"

calendarFormatToString Japanese = "japanese"

calendarFormatToString Persian = "persian"

calendarFormatToString Roc = "roc"

derive instance eqCalendarFormat :: Eq CalendarFormat

derive instance ordCalendarFormat :: Ord CalendarFormat

derive instance genericCalendarFormat :: Generic CalendarFormat _

instance decodeJsonCalendarFormat :: DecodeJson CalendarFormat where
  decodeJson = genericDecodeJson

instance encodeJsonCalendarFormat :: EncodeJson CalendarFormat where
  encodeJson = genericEncodeJson

instance showCalendarFormat :: Show CalendarFormat where
  show = genericShow

{-------------------------------------------------------------------------------
| ATTENTION: 18 is the number of values of `CalendarFormat`.
-}
instance arbitraryCalendarFormat :: Arbitrary CalendarFormat where
  arbitrary = map intToCalendarFormat arbitrary
    where
    intToCalendarFormat :: Int -> CalendarFormat
    intToCalendarFormat n
      | n >= 0 = case n `mod` 18 of
        0 -> Buddhist
        1 -> Chinese
        2 -> Coptic
        3 -> Dangi
        4 -> Ethioaa
        5 -> Ethiopic
        6 -> Gregory
        7 -> Hebrew
        8 -> Indian
        9 -> Islamic
        10 -> IslamicUmalqura
        11 -> IslamicTbla
        12 -> IslamicCivil
        13 -> IslamicRgsa
        14 -> Iso8601
        15 -> Japanese
        16 -> Persian
        _ -> Roc
      | otherwise = intToCalendarFormat (-n)
