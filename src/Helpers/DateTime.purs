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
  ( WeekDay(..)
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

{-------------------------------------------------------------------------------
| Return the current local year using 4 digits.
-}
foreign import getYearStringJS :: Unit -> Effect String

{-------------------------------------------------------------------------------
| Return the current local month using (at least) 2 digits.
-}
foreign import getMonthStringJS :: Unit -> Effect String

{-------------------------------------------------------------------------------
| Return the current local day of the month using (at least) 2 digits.
-}
foreign import getDayStringJS :: Unit -> Effect String

{-------------------------------------------------------------------------------
| Return the current local weekday as a number starting from 0 (sunday).
-}
foreign import getWeekDayStringJS :: Unit -> Effect Int

{-------------------------------------------------------------------------------
| Return the current local weekday as a `Weekday`.
-}
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

{-------------------------------------------------------------------------------
| Return the local hour of the day in 24h format.
-}
foreign import getHourStringJS :: Unit -> Effect String

{-------------------------------------------------------------------------------
| Return the local minute of the hour in 24h format.
-}
foreign import getMinuteStringJS :: Unit -> Effect String

{-------------------------------------------------------------------------------
| Return the local seconds of the minute in 24h format.
-}
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
