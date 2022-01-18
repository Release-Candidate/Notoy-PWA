-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2022 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     DateTime.purs
-- Date:     18.Jan.2022
--
-- ==============================================================================
-- | Module Helpers.DateTime, a wrapper for JS date and time functions.
module Helpers.DateTime
  ( DateStyle(..)
  , TimeStyle(..)
  , WeekDay(..)
  , getDateStringJS
  , getDayStringJS
  , getHourStringJS
  , getMinuteStringJS
  , getMonthStringJS
  , getSecondsStringJS
  , getTimeStringJS
  , getWeekDay
  , getYearStringJS
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Test.QuickCheck (class Arbitrary, arbitrary)

{-------------------------------------------------------------------------------
| Return the current local date as a string in ISO format, `YYYY-MM-DD`.
-}
foreign import getDateStringJS :: Unit -> Effect String

{-------------------------------------------------------------------------------
| Return the current local time as a string in 24h format, `HH:mm:ss`.
-}
foreign import getTimeStringJS :: Unit -> Effect String

foreign import getYearStringJS :: Unit -> Effect String

foreign import getMonthStringJS :: Unit -> Effect String

foreign import getDayStringJS :: Unit -> Effect String

foreign import getWeekDayStringJS :: Unit -> Effect Int

getWeekDay :: Effect WeekDay
getWeekDay = do
  weekday <- getWeekDayStringJS unit
  case weekday of
    0 -> pure Sunday
    1 -> pure Monday
    2 -> pure Tuesday
    3 -> pure Wednesday
    4 -> pure Thursday
    5 -> pure Friday
    _ -> pure Saturday

foreign import getHourStringJS :: Unit -> Effect String

foreign import getMinuteStringJS :: Unit -> Effect String

foreign import getSecondsStringJS :: Unit -> Effect String

{-------------------------------------------------------------------------------
| The days of a week.
|
| One of
|   * Monday
|   * Tuesday
|   * Wednesday
|   * Thursday
|   * Friday
|   * Saturday
|   * Sunday
-}
data WeekDay
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

derive instance eqWeekDay :: Eq WeekDay

derive instance ordWeekDay :: Ord WeekDay

derive instance genericWeekDay :: Generic WeekDay _

instance decodeJsonWeekDay :: DecodeJson WeekDay where
  decodeJson = genericDecodeJson

instance encodeJsonWeekDay :: EncodeJson WeekDay where
  encodeJson = genericEncodeJson

instance showWeekDay :: Show WeekDay where
  show = genericShow

{-------------------------------------------------------------------------------
| ATTENTION: 7 is the number of values of `WeekDay`.
-}
instance arbitraryWeekDay :: Arbitrary WeekDay where
  arbitrary = map intToWeekDay arbitrary
    where
    intToWeekDay :: Int -> WeekDay
    intToWeekDay n
      | n >= 0 = case n `mod` 7 of
        0 -> Monday
        1 -> Tuesday
        2 -> Wednesday
        3 -> Thursday
        4 -> Friday
        5 -> Saturday
        _ -> Sunday
      | otherwise = intToWeekDay (-n)

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
