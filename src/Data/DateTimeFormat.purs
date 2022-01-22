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
  , DateTimeFormatOptions(..)
  , DayFormat(..)
  , DayPeriod(..)
  , EraFormat(..)
  , FormatMatcher(..)
  , HourCycle(..)
  , Locale(..)
  , LocaleMatcher(..)
  , MonthFormat(..)
  , TimeStyle(..)
  , TimeZoneNameStyle(..)
  , WeekDayFormat(..)
  , YearFormat(..)
  , dateTimeFormat
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
import Data.Function.Uncurried (Fn2, Fn4, Fn6, Fn7, runFn2, runFn4, runFn6, runFn7)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)
import Untagged.Castable (cast)
import Untagged.Union (UndefinedOr)

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

{-------------------------------------------------------------------------------
| The options of a `DateTimeFormat`.
-}
data DateTimeFormatOptions
  = DateTimeFormatOptions
    { dateStyle :: Maybe DateStyle
    , timeStyle :: Maybe TimeStyle
    , calendar :: Maybe String
    , dayPeriod :: Maybe DayPeriod
    , numberingSystem :: Maybe String
    , localeMatcher :: Maybe LocaleMatcher
    , timeZone :: Maybe String
    , hour12 :: Maybe Boolean
    , hourCycle :: Maybe HourCycle
    , formatMatcher :: Maybe FormatMatcher
    , weekDay :: Maybe WeekDayFormat
    , era :: Maybe EraFormat
    , year :: Maybe YearFormat
    , month :: Maybe MonthFormat
    , day :: Maybe DayFormat
    -- , hour :: Maybe HourFormat
    -- , minute :: Maybe MinuteFormat
    -- , second :: Maybe SecondFormat
    -- , fractionalSecondDigits :: Maybe FractionalSecondDigits
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

dateTimeFormat :: Locale -> DateTimeFormatOptions -> DateTimeFormat
dateTimeFormat locale options = runFn2 getDateTimeFormatJS locale optionsJS
  where
  optionsJS = convertDateTimeOptions options

foreign import getDateTimeFormatJS :: Fn2 Locale DateTimeFormatOptionsJS DateTimeFormat

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
| The format of a day period - like `AM` and `PM`.
|
| 12 hour format must be enabled for this to have an effect.
|
| One of
|   * Narrow
|   * Short
|   * Long
-}
data DayPeriod
  = NarrowDP
  | ShortDP
  | LongDP

{-------------------------------------------------------------------------------
| The format of a day period, values for Javascript FFI.
-}
dayPeriodJS ::
  { long :: String
  , narrow :: String
  , short :: String
  }
dayPeriodJS = { narrow: "narrow", short: "short", long: "long" }

toDayPeriod :: DayPeriod -> String
toDayPeriod NarrowDP = dayPeriodJS.narrow

toDayPeriod ShortDP = dayPeriodJS.short

toDayPeriod LongDP = dayPeriodJS.long

derive instance eqDayPeriod :: Eq DayPeriod

derive instance ordDayPeriod :: Ord DayPeriod

derive instance genericDayPeriod :: Generic DayPeriod _

instance decodeJsonDayPeriod :: DecodeJson DayPeriod where
  decodeJson = genericDecodeJson

instance encodeJsonDayPeriod :: EncodeJson DayPeriod where
  encodeJson = genericEncodeJson

instance showDayPeriod :: Show DayPeriod where
  show = genericShow

{-------------------------------------------------------------------------------
| ATTENTION: 3 is the number of values of `DayPeriod`.
-}
instance arbitraryDayPeriod :: Arbitrary DayPeriod where
  arbitrary = map intToDayPeriod arbitrary
    where
    intToDayPeriod :: Int -> DayPeriod
    intToDayPeriod n
      | n >= 0 = case n `mod` 3 of
        0 -> NarrowDP
        1 -> ShortDP
        _ -> LongDP
      | otherwise = intToDayPeriod (-n)

{-------------------------------------------------------------------------------
| The options to match the locale.
|
| One of
|   * Lookup
|   * BestFit
-}
data LocaleMatcher
  = Lookup
  | BestFit

{-------------------------------------------------------------------------------
| The values of a `LocaleMatcher` for the JS FFI.
-}
localeMatcherJS ::
  { bestFit :: String
  , lookup :: String
  }
localeMatcherJS = { lookup: "lookup", bestFit: "best fit" }

toLocalMatcher :: LocaleMatcher -> String
toLocalMatcher Lookup = localeMatcherJS.lookup

toLocalMatcher BestFit = localeMatcherJS.bestFit

derive instance eqLocaleMatcher :: Eq LocaleMatcher

derive instance ordLocaleMatcher :: Ord LocaleMatcher

derive instance genericLocaleMatcher :: Generic LocaleMatcher _

instance decodeJsonLocaleMatcher :: DecodeJson LocaleMatcher where
  decodeJson = genericDecodeJson

instance encodeJsonLocaleMatcher :: EncodeJson LocaleMatcher where
  encodeJson = genericEncodeJson

instance showLocaleMatcher :: Show LocaleMatcher where
  show = genericShow

{-------------------------------------------------------------------------------
| ATTENTION: 2 is the number of values of `LocaleMatcher`.
-}
instance arbitraryLocaleMatcher :: Arbitrary LocaleMatcher where
  arbitrary = map intToLocaleMatcher arbitrary
    where
    intToLocaleMatcher :: Int -> LocaleMatcher
    intToLocaleMatcher n
      | n >= 0 = case n `mod` 2 of
        0 -> Lookup
        _ -> BestFit
      | otherwise = intToLocaleMatcher (-n)

{-------------------------------------------------------------------------------
| The number of hours in a clock cycle of a day.
|
| One of
|   * H11
|   * H12
|   * H23
|   * H24
-}
data HourCycle
  = H11
  | H12
  | H23
  | H24

{-------------------------------------------------------------------------------
| The values of `HourCycle` fo teh JS FFI.
-}
hourCycleJS ::
  { h11 :: String
  , h12 :: String
  , h23 :: String
  , h24 :: String
  }
hourCycleJS = { h11: "h11", h12: "h12", h23: "h23", h24: "h24" }

toHourCycle :: HourCycle -> String
toHourCycle H11 = hourCycleJS.h11

toHourCycle H12 = hourCycleJS.h12

toHourCycle H23 = hourCycleJS.h23

toHourCycle H24 = hourCycleJS.h24

derive instance eqHourCycle :: Eq HourCycle

derive instance ordHourCycle :: Ord HourCycle

derive instance genericHourCycle :: Generic HourCycle _

instance decodeJsonHourCycle :: DecodeJson HourCycle where
  decodeJson = genericDecodeJson

instance encodeJsonHourCycle :: EncodeJson HourCycle where
  encodeJson = genericEncodeJson

instance showHourCycle :: Show HourCycle where
  show = genericShow

{-------------------------------------------------------------------------------
| ATTENTION: 4 is the number of values of `HourCycle`.
-}
instance arbitraryHourCycle :: Arbitrary HourCycle where
  arbitrary = map intToHourCycle arbitrary
    where
    intToHourCycle :: Int -> HourCycle
    intToHourCycle n
      | n >= 0 = case n `mod` 4 of
        0 -> H11
        1 -> H12
        2 -> H23
        _ -> H24
      | otherwise = intToHourCycle (-n)

{-------------------------------------------------------------------------------
| The possible values of a format matcher.
|
| One of
|   * BasicFM
|   * BestFitFM
-}
data FormatMatcher
  = BasicFM
  | BestFitFM

{-------------------------------------------------------------------------------
| The values for the formatMatcher in the DateTimeFormat options.
-}
formatMatcherJS ::
  { basic :: String
  , bestFit :: String
  }
formatMatcherJS = { basic: "basic", bestFit: "best fit" }

toFormatMatcher :: FormatMatcher -> String
toFormatMatcher BasicFM = formatMatcherJS.basic

toFormatMatcher BestFitFM = formatMatcherJS.bestFit

derive instance eqFormatMatcher :: Eq FormatMatcher

derive instance ordFormatMatcher :: Ord FormatMatcher

derive instance genericFormatMatcher :: Generic FormatMatcher _

instance decodeJsonFormatMatcher :: DecodeJson FormatMatcher where
  decodeJson = genericDecodeJson

instance encodeJsonFormatMatcher :: EncodeJson FormatMatcher where
  encodeJson = genericEncodeJson

instance showFormatMatcher :: Show FormatMatcher where
  show = genericShow

{-------------------------------------------------------------------------------
| ATTENTION: 2 is the number of values of `FormatMatcher`.
-}
instance arbitraryFormatMatcher :: Arbitrary FormatMatcher where
  arbitrary = map intToFormatMatcher arbitrary
    where
    intToFormatMatcher :: Int -> FormatMatcher
    intToFormatMatcher n
      | n >= 0 = case n `mod` 2 of
        0 -> BasicFM
        _ -> BestFitFM
      | otherwise = intToFormatMatcher (-n)

{-------------------------------------------------------------------------------
| The possible values of a `WeekDayFormat`
|
| One of
|   * LongWD
|   * ShortWD
|   * NarrowWD
-}
data WeekDayFormat
  = LongWD
  | ShortWD
  | NarrowWD

{-------------------------------------------------------------------------------
| JS values of `WeekDayFormat`.
-}
weekDayFormatJS ::
  { long :: String
  , narrow :: String
  , short :: String
  }
weekDayFormatJS = { long: "long", short: "short", narrow: "narrow" }

toWeekDayFormat :: WeekDayFormat -> String
toWeekDayFormat LongWD = weekDayFormatJS.long

toWeekDayFormat ShortWD = weekDayFormatJS.short

toWeekDayFormat NarrowWD = weekDayFormatJS.narrow

derive instance eqWeekDayFormat :: Eq WeekDayFormat

derive instance ordWeekDayFormat :: Ord WeekDayFormat

derive instance genericWeekDayFormat :: Generic WeekDayFormat _

instance decodeJsonWeekDayFormat :: DecodeJson WeekDayFormat where
  decodeJson = genericDecodeJson

instance encodeJsonWeekDayFormat :: EncodeJson WeekDayFormat where
  encodeJson = genericEncodeJson

instance showWeekDayFormat :: Show WeekDayFormat where
  show = genericShow

{-------------------------------------------------------------------------------
| ATTENTION: 3 is the number of values of `WeekDayFormat`.
-}
instance arbitraryWeekDayFormat :: Arbitrary WeekDayFormat where
  arbitrary = map intToWeekDayFormat arbitrary
    where
    intToWeekDayFormat :: Int -> WeekDayFormat
    intToWeekDayFormat n
      | n >= 0 = case n `mod` 3 of
        0 -> LongWD
        1 -> ShortWD
        _ -> NarrowWD
      | otherwise = intToWeekDayFormat (-n)

{-------------------------------------------------------------------------------
| The format for the era of a date
|
| One of
|   * LongE
|   * ShortE
|   * NarrowE
-}
data EraFormat
  = LongE
  | ShortE
  | NarrowE

{-------------------------------------------------------------------------------
| Values of the era format for JS FFI.
-}
eraFormatJS ::
  { long :: String
  , narrow :: String
  , short :: String
  }
eraFormatJS = { long: "long", short: "short", narrow: "narrow" }

toEraFormat :: EraFormat -> String
toEraFormat LongE = eraFormatJS.long

toEraFormat ShortE = eraFormatJS.short

toEraFormat NarrowE = eraFormatJS.narrow

derive instance eqEraFormat :: Eq EraFormat

derive instance ordEraFormat :: Ord EraFormat

derive instance genericEraFormat :: Generic EraFormat _

instance decodeJsonEraFormat :: DecodeJson EraFormat where
  decodeJson = genericDecodeJson

instance encodeJsonEraFormat :: EncodeJson EraFormat where
  encodeJson = genericEncodeJson

instance showEraFormat :: Show EraFormat where
  show = genericShow

{-------------------------------------------------------------------------------
| ATTENTION: 3 is the number of values of `EraFormat`.
-}
instance arbitraryEraFormat :: Arbitrary EraFormat where
  arbitrary = map intToEraFormat arbitrary
    where
    intToEraFormat :: Int -> EraFormat
    intToEraFormat n
      | n >= 0 = case n `mod` 3 of
        0 -> LongE
        1 -> ShortE
        _ -> NarrowE
      | otherwise = intToEraFormat (-n)

{-------------------------------------------------------------------------------
| The format of a year in a date.
|
| One of
|   * NumericY
|   * TwoDigitY
-}
data YearFormat
  = NumericY
  | TwoDigitY

{-------------------------------------------------------------------------------
| The format of a year, JS FFI version.
-}
yearFormatJS ::
  { numeric :: String
  , twoDigit :: String
  }
yearFormatJS = { numeric: "numeric", twoDigit: "2-digit" }

toYearFormat :: YearFormat -> String
toYearFormat NumericY = yearFormatJS.numeric

toYearFormat TwoDigitY = yearFormatJS.twoDigit

derive instance eqYearFormat :: Eq YearFormat

derive instance ordYearFormat :: Ord YearFormat

derive instance genericYearFormat :: Generic YearFormat _

instance decodeJsonYearFormat :: DecodeJson YearFormat where
  decodeJson = genericDecodeJson

instance encodeJsonYearFormat :: EncodeJson YearFormat where
  encodeJson = genericEncodeJson

instance showYearFormat :: Show YearFormat where
  show = genericShow

{-------------------------------------------------------------------------------
| ATTENTION: 2 is the number of values of `YearFormat`.
-}
instance arbitraryYearFormat :: Arbitrary YearFormat where
  arbitrary = map intToYearFormat arbitrary
    where
    intToYearFormat :: Int -> YearFormat
    intToYearFormat n
      | n >= 0 = case n `mod` 2 of
        0 -> NumericY
        _ -> TwoDigitY
      | otherwise = intToYearFormat (-n)

{-------------------------------------------------------------------------------
| The format of a month.
|
| One of
|   * NumericM
|   * TwoDigitM
|   * LongM
|   * ShortM
|   * NarrowM
-}
data MonthFormat
  = NumericM
  | TwoDigitM
  | LongM
  | ShortM
  | NarrowM

{-------------------------------------------------------------------------------
| Month format in the JS version for FFI.
-}
monthFormatJS ∷
  { long ∷ String
  , narrow ∷ String
  , numeric ∷ String
  , short ∷ String
  , twoDigit ∷ String
  }
monthFormatJS =
  { numeric: "numeric"
  , twoDigit: "2-digit"
  , long: "long"
  , short: "short"
  , narrow: "narrow"
  }

toMonthFormat :: MonthFormat -> String
toMonthFormat NumericM = monthFormatJS.numeric

toMonthFormat TwoDigitM = monthFormatJS.twoDigit

toMonthFormat LongM = monthFormatJS.long

toMonthFormat ShortM = monthFormatJS.short

toMonthFormat NarrowM = monthFormatJS.narrow

derive instance eqMonthFormat :: Eq MonthFormat

derive instance ordMonthFormat :: Ord MonthFormat

derive instance genericMonthFormat :: Generic MonthFormat _

instance decodeJsonMonthFormat :: DecodeJson MonthFormat where
  decodeJson = genericDecodeJson

instance encodeJsonMonthFormat :: EncodeJson MonthFormat where
  encodeJson = genericEncodeJson

instance showMonthFormat :: Show MonthFormat where
  show = genericShow

{-------------------------------------------------------------------------------
| ATTENTION: 5 is the number of values of `MonthFormat`.
-}
instance arbitraryMonthFormat :: Arbitrary MonthFormat where
  arbitrary = map intToMonthFormat arbitrary
    where
    intToMonthFormat :: Int -> MonthFormat
    intToMonthFormat n
      | n >= 0 = case n `mod` 5 of
        0 -> NumericM
        1 -> TwoDigitM
        2 -> LongM
        3 -> ShortM
        _ -> NarrowM
      | otherwise = intToMonthFormat (-n)

{-------------------------------------------------------------------------------
| The format of a day
|
| One of
|   * NumericD
|   * TwoDigitD
-}
data DayFormat
  = NumericD
  | TwoDigitD

{-------------------------------------------------------------------------------
| The values of a day format for the JS FFI.
-}
dayFormatJS ::
  { numeric :: String
  , twoDigit :: String
  }
dayFormatJS = { numeric: "numeric", twoDigit: "2-digit" }

toDayFormat :: DayFormat -> String
toDayFormat NumericD = dayFormatJS.numeric

toDayFormat TwoDigitD = dayFormatJS.twoDigit

derive instance eqDayFormat :: Eq DayFormat

derive instance ordDayFormat :: Ord DayFormat

derive instance genericDayFormat :: Generic DayFormat _

instance decodeJsonDayFormat :: DecodeJson DayFormat where
  decodeJson = genericDecodeJson

instance encodeJsonDayFormat :: EncodeJson DayFormat where
  encodeJson = genericEncodeJson

instance showDayFormat :: Show DayFormat where
  show = genericShow

{-------------------------------------------------------------------------------
| ATTENTION: 2 is the number of values of `DayFormat`.
-}
instance arbitraryDayFormat :: Arbitrary DayFormat where
  arbitrary = map intToDayFormat arbitrary
    where
    intToDayFormat :: Int -> DayFormat
    intToDayFormat n
      | n >= 0 = case n `mod` 2 of
        0 -> NumericD
        _ -> TwoDigitD
      | otherwise = intToDayFormat (-n)

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
| The JS version of `DateTimeFormatOptions`, the record which is used for JS
| FFI.
-}
type DateTimeFormatOptionsJS
  = { dateStyle :: UndefinedOr String
    , timeStyle :: UndefinedOr String
    , calendar :: UndefinedOr String
    , dayPeriod :: UndefinedOr String
    , numberingSystem :: UndefinedOr String
    , localeMatcher :: UndefinedOr String
    , timeZone :: UndefinedOr String
    , hour12 :: UndefinedOr Boolean
    , hourCycle :: UndefinedOr String
    , formatMatcher :: UndefinedOr String
    , weekDay :: UndefinedOr String
    , era :: UndefinedOr String
    , year :: UndefinedOr String
    , month :: UndefinedOr String
    , day :: UndefinedOr String
    , hour :: UndefinedOr String
    , minute :: UndefinedOr String
    , second :: UndefinedOr String
    , fractionalSecondDigits :: UndefinedOr Int
    , timeZoneNameStyle :: UndefinedOr String
    }

{-------------------------------------------------------------------------------
| Helper function: convert a `DateTimeFormatOptions` object to a
| `DateTimeFormatOptionsJS`
-}
convertDateTimeOptions :: DateTimeFormatOptions -> DateTimeFormatOptionsJS
convertDateTimeOptions options = cast {}
