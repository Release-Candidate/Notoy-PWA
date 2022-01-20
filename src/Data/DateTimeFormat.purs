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
  , formatDate
  , formatDateInts
  , formatDateIntsUnsafe
  , formatDateTime
  , formatDateTimeInts
  , formatDateTimeIntsUnsafe
  , formatDateTimeNow
  , isoDateTime
  , isoDateTimeInts
  , isoDateTimeIntsUnsafe
  , isoDateTimeNow
  , localeToString
  , stringToLocale
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Date (Date, canonicalDate, day, month, year)
import Data.DateTime (DateTime(..), Time(..), hour, minute, second)
import Data.Enum (fromEnum, toEnum)
import Data.Function.Uncurried (Fn4, Fn6, Fn7, runFn4, runFn6, runFn7)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)

{-------------------------------------------------------------------------------
| Type to hold the locale and formatting options of a date or a time.
-}
foreign import data DateTimeFormat :: Type

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
| The options of a `DateTimeFormat`.
-}
data DateTimeFormatOptions
  = DateTimeFormatOptions
    { dateStyle :: Maybe DateStyle
    , timeStyle :: Maybe TimeStyle
    , timeZoneNameStyle :: Maybe TimeZoneNameStyle
    }

derive instance eqDateTimeFormatOptions :: Eq DateTimeFormatOptions

derive instance ordDateTimeFormatOptions :: Ord DateTimeFormatOptions

derive instance genericDateTimeFormatOptions :: Generic DateTimeFormatOptions _

instance decodeJsonDateTimeFormatOptions :: DecodeJson DateTimeFormatOptions where
  decodeJson = genericDecodeJson

instance encodeJsonDateTimeFormatOptions :: EncodeJson DateTimeFormatOptions where
  encodeJson = genericEncodeJson

instance showDateTimeFormatOptions :: Show DateTimeFormatOptions where
  show = genericShow

instance arbitraryDateTimeFormatOptions :: Arbitrary DateTimeFormatOptions where
  arbitrary = genericArbitrary

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
| Return the current date and time as ISO ISO 8601 string as UTC.
-}
foreign import isoDateTimeNow :: Unit -> Effect String

foreign import isoDateTimeJS :: Fn6 Int Int Int Int Int Int String

{-------------------------------------------------------------------------------
| Return the given date and time as ISO ISO 8601 string as UTC.
|
| * `dateTime` - The local date and time to return as ISO ISO 8601 string.
-}
isoDateTime :: DateTime -> String
isoDateTime (DateTime date time) = runFn6 isoDateTimeJS year' month' day' hour' minute' seconds'
  where
  year' = fromEnum $ year date

  month' = (fromEnum $ month date) - 1

  day' = fromEnum $ day date

  hour' = fromEnum $ hour time

  minute' = fromEnum $ minute time

  seconds' = fromEnum $ second time

{-------------------------------------------------------------------------------
| Return the given local date and time as ISO ISO 8601 string as UTC.
|
| All bounds are checked, but for dates like the `31st of November` the
| `1st of December` is returned. `30th February` yields `2nd (or 1st) March`.
| A date like the `52th of November` yields `Nothing`.
|
| * year - The Year
| * month - The month (starting at 1, January is 1)
| * day - The day of the month (starting at 1)
| * hour - The hour
| * minutes - The minutes
| * seconds - The seconds
-}
isoDateTimeInts ::
  Int -> Int -> Int -> Int -> Int -> Int -> Maybe String
isoDateTimeInts yearI monthI dayI hourI minuteI secondsI = do
  year' <- toEnum yearI
  month' <- toEnum monthI
  day' <- toEnum dayI
  hour' <- toEnum hourI
  minute' <- toEnum minuteI
  seconds' <- toEnum secondsI
  millis' <- toEnum 0
  let
    date = canonicalDate year' month' day'

    time = Time hour' minute' seconds' millis'
  pure $ isoDateTime (DateTime date time)

{-------------------------------------------------------------------------------
| Return the given date and time as ISO ISO 8601 string as UTC.
|
| No bounds are checked, a date like the `52th of November` yields
| `22nd of December`.
|
| * year - The Year
| * month - The month (starting at 1, January is 1)
| * day - The day of the month (starting at 1)
| * hour - The hour
| * minutes - The minutes
| * seconds - The seconds
-}
isoDateTimeIntsUnsafe ::
  Int -> Int -> Int -> Int -> Int -> Int -> String
isoDateTimeIntsUnsafe yearI monthI dayI hourI minuteI secondsI = runFn6 isoDateTimeJS yearI (monthI - 1) dayI hourI minuteI secondsI

foreign import formatDateJS :: Fn4 DateTimeFormat Int Int Int String

{-------------------------------------------------------------------------------
| Return the given local date formatted by the given formatter.
|
| All bounds are checked, but for dates like the `31st of November` the
| `1st of December` is returned. `30th February` yields `2nd (or 1st) March`.
| A date like the `52th of November` yields `Nothing`.
|
| * formatter - The format to use
| * date - The local date to format
-}
formatDate :: DateTimeFormat -> Date -> String
formatDate formatter date = runFn4 formatDateJS formatter year' month' day'
  where
  year' = fromEnum $ year date

  month' = (fromEnum $ month date) - 1

  day' = fromEnum $ day date

{-------------------------------------------------------------------------------
| Return the given local date formatted by the given formatter.
|
| All bounds are checked, but for dates like the `31st of November` the
| `1st of December` is returned. `30th February` yields `2nd (or 1st) March`.
| A date like the `52th of November` yields `Nothing`.
|
| * formatter - The format to use
| * year - The Year
| * month - The month (starting at 1, January is 1)
| * day - The day of the month (starting at 1)
-}
formatDateInts :: DateTimeFormat -> Int -> Int -> Int -> Maybe String
formatDateInts formatter yearI monthI dayI = do
  year' <- toEnum yearI
  month' <- toEnum monthI
  day' <- toEnum dayI
  let
    date = canonicalDate year' month' day'
  pure $ formatDate formatter date

{-------------------------------------------------------------------------------
| Return the given date formatted by the given formatter.
|
| No bounds are checked, a date like the `52th of November` yields
| `22nd of December`.
|
| * formatter - The format to use
| * year - The Year
| * month - The month (starting at 1, January is 1)
| * day - The day of the month (starting at 1)
-}
formatDateIntsUnsafe :: DateTimeFormat -> Int -> Int -> Int -> String
formatDateIntsUnsafe formatter yearI monthI dayI = runFn4 formatDateJS formatter yearI (monthI - 1) dayI

foreign import formatDateTimeJS :: Fn7 DateTimeFormat Int Int Int Int Int Int String

{-------------------------------------------------------------------------------
| Return the given local date and time formatted by the given formatter.
|
| All bounds are checked, but for dates like the `31st of November` the
| `1st of December` is returned. `30th February` yields `2nd (or 1st) March`.
| A date like the `52th of November` yields `Nothing`.
|
| * formatter - The format to use
| * dateTime - The local date and time to format
-}
formatDateTime :: DateTimeFormat -> DateTime -> String
formatDateTime formatter (DateTime date time) = runFn7 formatDateTimeJS formatter year' month' day' hour' minute' seconds'
  where
  year' = fromEnum $ year date

  month' = (fromEnum $ month date) - 1

  day' = fromEnum $ day date

  hour' = fromEnum $ hour time

  minute' = fromEnum $ minute time

  seconds' = fromEnum $ second time

{-------------------------------------------------------------------------------
| Return the given local date and time formatted by the given formatter.
|
| All bounds are checked, but for dates like the `31st of November` the
| `1st of December` is returned. `30th February` yields `2nd (or 1st) March`.
| A date like the `52th of November` yields `Nothing`.
|
| * formatter - The format to use
| * year - The Year
| * month - The month (starting at 1, January is 1)
| * day - The day of the month (starting at 1)
| * hour - The hour
| * minutes - The minutes
| * seconds - The seconds
-}
formatDateTimeInts ::
  DateTimeFormat -> Int -> Int -> Int -> Int -> Int -> Int -> Maybe String
formatDateTimeInts formatter yearI monthI dayI hourI minuteI secondsI = do
  year' <- toEnum yearI
  month' <- toEnum monthI
  day' <- toEnum dayI
  hour' <- toEnum hourI
  minute' <- toEnum minuteI
  seconds' <- toEnum secondsI
  millis' <- toEnum 0
  let
    date = canonicalDate year' month' day'

    time = Time hour' minute' seconds' millis'
  pure $ formatDateTime formatter (DateTime date time)

{-------------------------------------------------------------------------------
| Return the given date and time formatted by the given formatter.
|
| No bounds are checked, a date like the `52th of November` yields
| `22nd of December`.
|
| * formatter - The format to use
| * year - The Year
| * month - The month (starting at 1, January is 1)
| * day - The day of the month (starting at 1)
| * hour - The hour
| * minutes - The minutes
| * seconds - The seconds
-}
formatDateTimeIntsUnsafe ::
  DateTimeFormat -> Int -> Int -> Int -> Int -> Int -> Int -> String
formatDateTimeIntsUnsafe formatter yearI monthI dayI hourI minuteI secondsI = runFn7 formatDateTimeJS formatter yearI (monthI - 1) dayI hourI minuteI secondsI

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

{-------------------------------------------------------------------------------
| The possible values of the `timeStyle` field in the Javascript options record.
-}
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
