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
|   *buddhist
Thai Buddhist calendar

chinese
Traditional Chinese calendar

coptic
Coptic calendar

dangi
Traditional Korean calendar

ethioaa
Ethiopic calendar, Amete Alem (epoch approx. 5493 B.C.E)

ethiopic
Ethiopic calendar, Amete Mihret (epoch approx, 8 C.E.)

gregory
Gregorian calendar

hebrew
Traditional Hebrew calendar

indian
Indian calendar

islamic
Islamic calendar

islamic-umalqura
Islamic calendar, Umm al-Qura

islamic-tbla
Islamic calendar, tabular (intercalary years [2,5,7,10,13,16,18,21,24,26,29] - astronomical epoch)

islamic-civil
Islamic calendar, tabular (intercalary years [2,5,7,10,13,16,18,21,24,26,29] - civil epoch)

islamic-rgsa
Islamic calendar, Saudi Arabia sighting

iso8601
ISO calendar (Gregorian calendar using the ISO 8601 calendar week rules)

japanese
Japanese Imperial calendar

persian
Persian calendar

roc
Civil (algorithmic) Arabic calendar

islamicc
-}
data CalendarFormat
  = A
  | B

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
        0 -> A
        _ -> B
      | otherwise = intToCalendarFormat (-n)
