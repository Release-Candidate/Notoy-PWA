-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2022 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     DateTimeFormat.purs
-- Date:     20.Jan.2022
--
-- ==============================================================================
-- | Module Data.DateTimeFormat, to set the locale and format of a date or time.
-- | See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat/DateTimeFormat
module Data.DateTimeFormat
  ( DateStyle(..)
  , DateTimeFormat
  , DateTimeFormatOptions(..)
  , DayFormat(..)
  , DayPeriod(..)
  , EraFormat(..)
  , FormatMatcher(..)
  , FractionalSecondDigits(..)
  , Hour12(..)
  , HourCycle(..)
  , HourFormat(..)
  , Locale(..)
  , LocaleMatcher(..)
  , MinuteFormat(..)
  , MonthFormat(..)
  , SecondFormat(..)
  , TimeStyle(..)
  , TimeZone(..)
  , TimeZoneNameStyle(..)
  , WeekDayFormat(..)
  , YearFormat(..)
  , dateTimeFormat
  , defaultDateTimeFormat
  , emptyDateTimeFormatOptions
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
  , setCalendarFormat
  , setDateStyle
  , setDayFormat
  , setDayPeriod
  , setEraFormat
  , setFormatMatcher
  , setFractionalSecondDigits
  , setHour12
  , setHourCycle
  , setHourFormat
  , setLocaleMatcher
  , setMinuteFormat
  , setMonthFormat
  , setNumberingSystem
  , setSecondFormat
  , setTimeStyle
  , setTimeZone
  , setTimeZoneNameStyle
  , setWeekDayFormat
  , setYearFormat
  , stringToLocale
  , stringToLocaleUnsafe
  , stringToTimeZone
  , timeZoneToString
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.CalendarFormat (CalendarFormat, calendarFormatToString)
import Data.Date (Date, canonicalDate, day, month, year)
import Data.DateTime (DateTime(..), Time(..), hour, minute, second)
import Data.Enum (fromEnum, toEnum)
import Data.Function.Uncurried (Fn2, Fn4, Fn6, Fn7, runFn2, runFn4, runFn6, runFn7)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.NumberingSystem (NumberingSystem, numberingSystemToString)
import Data.Show.Generic (genericShow)
import Data.String (trim)
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags (unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
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
| This is a wrapped BCP 47 with Unicode extensions ('Extension U') locale tag.
|
| Examples of valid locale strings:
|
| "en", "zh", "de"
| "eng", "zho", "deu"
| "en-US", "zh-cn", "de-AT"
| "en-US-u-ca-buddhist", "gsw-u-sd-chzh", "he-IL-u-ca-hebrew-tz-jeruslm"
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
| Convert a BCP 47 language tag `String` to a `Locale`.
|
| The given `String` is not validated, every non-empty String containing any
| non-whitespace character returns a `Locale`!
|
| * `str` - The `String` to convert to a `Locale`.
-}
stringToLocale :: String -> Maybe Locale
stringToLocale "" = Nothing

stringToLocale str = case match notOnlyWhitespaceRegex str of
  Nothing -> Nothing
  _ -> Just $ wrap $ trim str

{-------------------------------------------------------------------------------
| Convert a BCP 47 language tag `String` to a `Locale`.
|
| The given `String` is not validated, every String returns a `Locale`!
|
| * `str` - The `String` to convert to a `Locale`.
-}
stringToLocaleUnsafe :: String -> Locale
stringToLocaleUnsafe = wrap

{-------------------------------------------------------------------------------
| A empty `DateTimeFormatOptions` object, with all fields set to `Nothing`.
|
| This is also the `Monoid`s `mempty`.
-}
emptyDateTimeFormatOptions ∷ DateTimeFormatOptions
emptyDateTimeFormatOptions =
  DateTimeFormatOptions
    { dateStyle: Nothing
    , timeStyle: Nothing
    , calendar: Nothing
    , dayPeriod: Nothing
    , numberingSystem: Nothing
    , localeMatcher: Nothing
    , timeZone: Nothing
    , hour12: Nothing
    , hourCycle: Nothing
    , formatMatcher: Nothing
    , weekDay: Nothing
    , era: Nothing
    , year: Nothing
    , month: Nothing
    , day: Nothing
    , hour: Nothing
    , minute: Nothing
    , second: Nothing
    , fractionalSecondDigits: Nothing
    , timeZoneNameStyle: Nothing
    }

{-------------------------------------------------------------------------------
| Set the `dateStyle` field of the given `DateTimeFormatOptions` to the given
| value.
-}
setDateStyle :: DateStyle -> DateTimeFormatOptions -> DateTimeFormatOptions
setDateStyle style (DateTimeFormatOptions options) = DateTimeFormatOptions options { dateStyle = Just style }

{-------------------------------------------------------------------------------
| Set the `timeStyle` field of the given `DateTimeFormatOptions` to the given
| value.
-}
setTimeStyle :: TimeStyle -> DateTimeFormatOptions -> DateTimeFormatOptions
setTimeStyle style (DateTimeFormatOptions options) = DateTimeFormatOptions options { timeStyle = Just style }

{-------------------------------------------------------------------------------
| Set the `calendar` field of the given `DateTimeFormatOptions` to the given
| value.
-}
setCalendarFormat :: CalendarFormat -> DateTimeFormatOptions -> DateTimeFormatOptions
setCalendarFormat style (DateTimeFormatOptions options) = DateTimeFormatOptions options { calendar = Just style }

{-------------------------------------------------------------------------------
| Set the `dayPeriod` field of the given `DateTimeFormatOptions` to the given
| value.
-}
setDayPeriod :: DayPeriod -> DateTimeFormatOptions -> DateTimeFormatOptions
setDayPeriod style (DateTimeFormatOptions options) = DateTimeFormatOptions options { dayPeriod = Just style }

{-------------------------------------------------------------------------------
| Set the `numberingSystem` field of the given `DateTimeFormatOptions` to the given
| value.
-}
setNumberingSystem :: NumberingSystem -> DateTimeFormatOptions -> DateTimeFormatOptions
setNumberingSystem style (DateTimeFormatOptions options) = DateTimeFormatOptions options { numberingSystem = Just style }

{-------------------------------------------------------------------------------
| Set the `localeMatcher` field of the given `DateTimeFormatOptions` to the given
| value.
-}
setLocaleMatcher :: LocaleMatcher -> DateTimeFormatOptions -> DateTimeFormatOptions
setLocaleMatcher style (DateTimeFormatOptions options) = DateTimeFormatOptions options { localeMatcher = Just style }

{-------------------------------------------------------------------------------
| Set the `timeZone` field of the given `DateTimeFormatOptions` to the given
| value.
-}
setTimeZone :: TimeZone -> DateTimeFormatOptions -> DateTimeFormatOptions
setTimeZone style (DateTimeFormatOptions options) = DateTimeFormatOptions options { timeZone = Just style }

{-------------------------------------------------------------------------------
| Set the `hour12` field of the given `DateTimeFormatOptions` to the given
| value.
-}
setHour12 :: Hour12 -> DateTimeFormatOptions -> DateTimeFormatOptions
setHour12 style (DateTimeFormatOptions options) = DateTimeFormatOptions options { hour12 = Just style }

{-------------------------------------------------------------------------------
| Set the `hourCycle` field of the given `DateTimeFormatOptions` to the given
| value.
-}
setHourCycle :: HourCycle -> DateTimeFormatOptions -> DateTimeFormatOptions
setHourCycle style (DateTimeFormatOptions options) = DateTimeFormatOptions options { hourCycle = Just style }

{-------------------------------------------------------------------------------
| Set the `formatMatcher` field of the given `DateTimeFormatOptions` to the given
| value.
-}
setFormatMatcher :: FormatMatcher -> DateTimeFormatOptions -> DateTimeFormatOptions
setFormatMatcher style (DateTimeFormatOptions options) = DateTimeFormatOptions options { formatMatcher = Just style }

{-------------------------------------------------------------------------------
| Set the `weekDay` field of the given `DateTimeFormatOptions` to the given
| value.
-}
setWeekDayFormat :: WeekDayFormat -> DateTimeFormatOptions -> DateTimeFormatOptions
setWeekDayFormat style (DateTimeFormatOptions options) = DateTimeFormatOptions options { weekDay = Just style }

{-------------------------------------------------------------------------------
| Set the `era` field of the given `DateTimeFormatOptions` to the given
| value.
-}
setEraFormat :: EraFormat -> DateTimeFormatOptions -> DateTimeFormatOptions
setEraFormat style (DateTimeFormatOptions options) = DateTimeFormatOptions options { era = Just style }

{-------------------------------------------------------------------------------
| Set the `year` field of the given `DateTimeFormatOptions` to the given
| value.
-}
setYearFormat :: YearFormat -> DateTimeFormatOptions -> DateTimeFormatOptions
setYearFormat style (DateTimeFormatOptions options) = DateTimeFormatOptions options { year = Just style }

{-------------------------------------------------------------------------------
| Set the `month` field of the given `DateTimeFormatOptions` to the given
| value.
-}
setMonthFormat :: MonthFormat -> DateTimeFormatOptions -> DateTimeFormatOptions
setMonthFormat style (DateTimeFormatOptions options) = DateTimeFormatOptions options { month = Just style }

{-------------------------------------------------------------------------------
| Set the `day` field of the given `DateTimeFormatOptions` to the given
| value.
-}
setDayFormat :: DayFormat -> DateTimeFormatOptions -> DateTimeFormatOptions
setDayFormat style (DateTimeFormatOptions options) = DateTimeFormatOptions options { day = Just style }

{-------------------------------------------------------------------------------
| Set the `hour` field of the given `DateTimeFormatOptions` to the given
| value.
-}
setHourFormat :: HourFormat -> DateTimeFormatOptions -> DateTimeFormatOptions
setHourFormat style (DateTimeFormatOptions options) = DateTimeFormatOptions options { hour = Just style }

{-------------------------------------------------------------------------------
| Set the `minute` field of the given `DateTimeFormatOptions` to the given
| value.
-}
setMinuteFormat :: MinuteFormat -> DateTimeFormatOptions -> DateTimeFormatOptions
setMinuteFormat style (DateTimeFormatOptions options) = DateTimeFormatOptions options { minute = Just style }

{-------------------------------------------------------------------------------
| Set the `second` field of the given `DateTimeFormatOptions` to the given
| value.
-}
setSecondFormat :: SecondFormat -> DateTimeFormatOptions -> DateTimeFormatOptions
setSecondFormat style (DateTimeFormatOptions options) = DateTimeFormatOptions options { second = Just style }

{-------------------------------------------------------------------------------
| Set the `fractionalSecondDigits` field of the given `DateTimeFormatOptions` to the given
| value.
-}
setFractionalSecondDigits :: FractionalSecondDigits -> DateTimeFormatOptions -> DateTimeFormatOptions
setFractionalSecondDigits style (DateTimeFormatOptions options) = DateTimeFormatOptions options { fractionalSecondDigits = Just style }

{-------------------------------------------------------------------------------
| Set the `timeZoneNameStyle` field of the given `DateTimeFormatOptions` to the given
| value.
-}
setTimeZoneNameStyle :: TimeZoneNameStyle -> DateTimeFormatOptions -> DateTimeFormatOptions
setTimeZoneNameStyle style (DateTimeFormatOptions options) = DateTimeFormatOptions options { timeZoneNameStyle = Just style }

{-------------------------------------------------------------------------------
| The options of a `DateTimeFormat`.
|
| You can set the fields of `DateTimeFormat` using `Monoid`s `append` `<>`, or by
| chaining calls to the `set...` functions using `#`.
|
| Example:
|
| ```purescript
| formatOpts = setDateStyle (DateStyle Long) mempty <> setTimeStyle (TimeStyle MediumT) mempty
|
| formatOpts = mempty # setDateStyle (DateStyle Long) # setTimeStyle (TimeStyle MediumT)
| ```
|
| See MDN for more information of the fields:
| https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat/DateTimeFormat
-}
newtype DateTimeFormatOptions
  = DateTimeFormatOptions
  { dateStyle :: Maybe DateStyle
  , timeStyle :: Maybe TimeStyle
  , calendar :: Maybe CalendarFormat
  , dayPeriod :: Maybe DayPeriod
  , numberingSystem :: Maybe NumberingSystem
  , localeMatcher :: Maybe LocaleMatcher
  , timeZone :: Maybe TimeZone
  , hour12 :: Maybe Hour12
  , hourCycle :: Maybe HourCycle
  , formatMatcher :: Maybe FormatMatcher
  , weekDay :: Maybe WeekDayFormat
  , era :: Maybe EraFormat
  , year :: Maybe YearFormat
  , month :: Maybe MonthFormat
  , day :: Maybe DayFormat
  , hour :: Maybe HourFormat
  , minute :: Maybe MinuteFormat
  , second :: Maybe SecondFormat
  , fractionalSecondDigits :: Maybe FractionalSecondDigits
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

instance semiGroupDateTimeFormatOptions :: Semigroup DateTimeFormatOptions where
  append (DateTimeFormatOptions a) (DateTimeFormatOptions b) =
    DateTimeFormatOptions
      { dateStyle: setAOrB a.dateStyle b.dateStyle
      , timeStyle: setAOrB a.timeStyle b.timeStyle
      , calendar: setAOrB a.calendar b.calendar
      , dayPeriod: setAOrB a.dayPeriod b.dayPeriod
      , numberingSystem: setAOrB a.numberingSystem b.numberingSystem
      , localeMatcher: setAOrB a.localeMatcher b.localeMatcher
      , timeZone: setAOrB a.timeZone b.timeZone
      , hour12: setAOrB a.hour12 b.hour12
      , hourCycle: setAOrB a.hourCycle b.hourCycle
      , formatMatcher: setAOrB a.formatMatcher b.formatMatcher
      , weekDay: setAOrB a.weekDay b.weekDay
      , era: setAOrB a.era b.era
      , year: setAOrB a.year b.year
      , month: setAOrB a.month b.month
      , day: setAOrB a.day b.day
      , hour: setAOrB a.hour b.hour
      , minute: setAOrB a.minute b.minute
      , second: setAOrB a.second b.second
      , fractionalSecondDigits: setAOrB a.fractionalSecondDigits b.fractionalSecondDigits
      , timeZoneNameStyle: setAOrB a.timeZoneNameStyle b.timeZoneNameStyle
      }
    where
    setAOrB :: forall a. Maybe a -> Maybe a -> Maybe a
    setAOrB c Nothing = c

    setAOrB _ (Just d) = Just d

instance monoidDateTimeFormatOptions :: Monoid DateTimeFormatOptions where
  mempty = emptyDateTimeFormatOptions

{-------------------------------------------------------------------------------
| Return the default `DateTimeFormat` object of the current (browser) locale.
-}
foreign import defaultDateTimeFormat :: Unit -> Effect DateTimeFormat

{-------------------------------------------------------------------------------
| Return a `DateTimeFormat` from the given `Locale` and `DateTimeFormatOptions`.
|
| This format is to be used as argument to to `format...` functions, like
| `formatDateTime`.
-}
dateTimeFormat :: Array Locale -> DateTimeFormatOptions -> DateTimeFormat
dateTimeFormat locales options = runFn2 getDateTimeFormatJS locales optionsJS
  where
  optionsJS = convertDateTimeOptions options

foreign import getDateTimeFormatJS :: Fn2 (Array Locale) DateTimeFormatOptionsJS DateTimeFormat

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
 ATTENTION: 4 is the number of values of `DateStyle`.
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
 ATTENTION: 4 is the number of values of `TimeStyle`.
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

toDayPeriodJS :: DayPeriod -> String
toDayPeriodJS NarrowDP = dayPeriodJS.narrow

toDayPeriodJS ShortDP = dayPeriodJS.short

toDayPeriodJS LongDP = dayPeriodJS.long

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
 ATTENTION: 3 is the number of values of `DayPeriod`.
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

toLocalMatcherJS :: LocaleMatcher -> String
toLocalMatcherJS Lookup = localeMatcherJS.lookup

toLocalMatcherJS BestFit = localeMatcherJS.bestFit

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
 ATTENTION: 2 is the number of values of `LocaleMatcher`.
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
| The timezone of a locale.
|
| See https://www.iana.org/time-zones for (long ;) list.
-}
newtype TimeZone
  = TimeZone String

derive newtype instance eqTimeZone :: Eq TimeZone

derive newtype instance ordTimeZone :: Ord TimeZone

derive newtype instance showTimeZone :: Show TimeZone

derive newtype instance decodeJsonTimeZone :: DecodeJson TimeZone

derive newtype instance encodeJsonTimeZone :: EncodeJson TimeZone

derive instance genericTimeZone :: Generic TimeZone _

derive instance newtypeTimeZone :: Newtype TimeZone _

derive newtype instance arbitraryTimeZone :: Arbitrary TimeZone

{-------------------------------------------------------------------------------
| Convert a `TimeZone` to a `String`.
|
| * `obj` - The `TimeZone` to convert to a `String`.
-}
timeZoneToString :: TimeZone -> String
timeZoneToString = unwrap

{-------------------------------------------------------------------------------
| Convert a String to a `TimeZone`.
|
| * `obj` - The `String` to convert to a `TimeZone`.
-}
stringToTimeZone :: String -> TimeZone
stringToTimeZone = wrap

{-------------------------------------------------------------------------------
| Set the time formt to a 12 hour one.
|
| Only works if the number of hours isn't set to 24 elsewhere.
|
| One of
|   * Hour12
|   * Hour24
-}
data Hour12
  = Hour12
  | Hour24

derive instance eqHour12 :: Eq Hour12

derive instance ordHour12 :: Ord Hour12

derive instance genericHour12 :: Generic Hour12 _

instance decodeJsonHour12 :: DecodeJson Hour12 where
  decodeJson = genericDecodeJson

instance encodeJsonHour12 :: EncodeJson Hour12 where
  encodeJson = genericEncodeJson

instance showHour12 :: Show Hour12 where
  show = genericShow

{-------------------------------------------------------------------------------
 ATTENTION: 2 is the number of values of `Hour12`.
-}
instance arbitraryHour12 :: Arbitrary Hour12 where
  arbitrary = map intToHour12 arbitrary
    where
    intToHour12 :: Int -> Hour12
    intToHour12 n
      | n >= 0 = case n `mod` 2 of
        0 -> Hour12
        _ -> Hour24
      | otherwise = intToHour12 (-n)

hour12ToBoolean :: Hour12 -> Boolean
hour12ToBoolean Hour12 = true

hour12ToBoolean Hour24 = false

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

toHourCycleJS :: HourCycle -> String
toHourCycleJS H11 = hourCycleJS.h11

toHourCycleJS H12 = hourCycleJS.h12

toHourCycleJS H23 = hourCycleJS.h23

toHourCycleJS H24 = hourCycleJS.h24

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
 ATTENTION: 4 is the number of values of `HourCycle`.
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

toFormatMatcherJS :: FormatMatcher -> String
toFormatMatcherJS BasicFM = formatMatcherJS.basic

toFormatMatcherJS BestFitFM = formatMatcherJS.bestFit

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
 ATTENTION: 2 is the number of values of `FormatMatcher`.
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

toWeekDayFormatJS :: WeekDayFormat -> String
toWeekDayFormatJS LongWD = weekDayFormatJS.long

toWeekDayFormatJS ShortWD = weekDayFormatJS.short

toWeekDayFormatJS NarrowWD = weekDayFormatJS.narrow

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
 ATTENTION: 3 is the number of values of `WeekDayFormat`.
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

toEraFormatJS :: EraFormat -> String
toEraFormatJS LongE = eraFormatJS.long

toEraFormatJS ShortE = eraFormatJS.short

toEraFormatJS NarrowE = eraFormatJS.narrow

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
 ATTENTION: 3 is the number of values of `EraFormat`.
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

toYearFormatJS :: YearFormat -> String
toYearFormatJS NumericY = yearFormatJS.numeric

toYearFormatJS TwoDigitY = yearFormatJS.twoDigit

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
 ATTENTION: 2 is the number of values of `YearFormat`.
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

toMonthFormatJS :: MonthFormat -> String
toMonthFormatJS NumericM = monthFormatJS.numeric

toMonthFormatJS TwoDigitM = monthFormatJS.twoDigit

toMonthFormatJS LongM = monthFormatJS.long

toMonthFormatJS ShortM = monthFormatJS.short

toMonthFormatJS NarrowM = monthFormatJS.narrow

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
 ATTENTION: 5 is the number of values of `MonthFormat`.
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

toDayFormatJS :: DayFormat -> String
toDayFormatJS NumericD = dayFormatJS.numeric

toDayFormatJS TwoDigitD = dayFormatJS.twoDigit

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
 ATTENTION: 2 is the number of values of `DayFormat`.
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
| The format fo an hour.
|
| One of
|   * NumericH
|   * TwoDigitH
-}
data HourFormat
  = NumericH
  | TwoDigitH

{-------------------------------------------------------------------------------
| The format of an hour, for JS FFI.
-}
hourFormatJS ::
  { numeric :: String
  , twoDigit :: String
  }
hourFormatJS = { numeric: "numeric", twoDigit: "2-digit" }

toHourFormatJS :: HourFormat -> String
toHourFormatJS NumericH = hourFormatJS.numeric

toHourFormatJS TwoDigitH = hourFormatJS.twoDigit

derive instance eqHourFormat :: Eq HourFormat

derive instance ordHourFormat :: Ord HourFormat

derive instance genericHourFormat :: Generic HourFormat _

instance decodeJsonHourFormat :: DecodeJson HourFormat where
  decodeJson = genericDecodeJson

instance encodeJsonHourFormat :: EncodeJson HourFormat where
  encodeJson = genericEncodeJson

instance showHourFormat :: Show HourFormat where
  show = genericShow

{-------------------------------------------------------------------------------
 ATTENTION: 2 is the number of values of `HourFormat`.
-}
instance arbitraryHourFormat :: Arbitrary HourFormat where
  arbitrary = map intToHourFormat arbitrary
    where
    intToHourFormat :: Int -> HourFormat
    intToHourFormat n
      | n >= 0 = case n `mod` 2 of
        0 -> NumericH
        _ -> TwoDigitH
      | otherwise = intToHourFormat (-n)

{-------------------------------------------------------------------------------
| The format of a minute.
|
| One of
|   * NumericMi
|   * TwoDigitMi
-}
data MinuteFormat
  = NumericMi
  | TwoDigitMi

{-------------------------------------------------------------------------------
| The minute format, JS interop.
-}
minuteFormatJS ::
  { numeric :: String
  , twoDigit :: String
  }
minuteFormatJS = { numeric: "numeric", twoDigit: "2-digit" }

toMinuteFormatJS :: MinuteFormat -> String
toMinuteFormatJS NumericMi = minuteFormatJS.numeric

toMinuteFormatJS TwoDigitMi = minuteFormatJS.twoDigit

derive instance eqMinuteFormat :: Eq MinuteFormat

derive instance ordMinuteFormat :: Ord MinuteFormat

derive instance genericMinuteFormat :: Generic MinuteFormat _

instance decodeJsonMinuteFormat :: DecodeJson MinuteFormat where
  decodeJson = genericDecodeJson

instance encodeJsonMinuteFormat :: EncodeJson MinuteFormat where
  encodeJson = genericEncodeJson

instance showMinuteFormat :: Show MinuteFormat where
  show = genericShow

{-------------------------------------------------------------------------------
 ATTENTION: 2 is the number of values of `MinuteFormat`.
-}
instance arbitraryMinuteFormat :: Arbitrary MinuteFormat where
  arbitrary = map intToMinuteFormat arbitrary
    where
    intToMinuteFormat :: Int -> MinuteFormat
    intToMinuteFormat n
      | n >= 0 = case n `mod` 2 of
        0 -> NumericMi
        _ -> TwoDigitMi
      | otherwise = intToMinuteFormat (-n)

{-------------------------------------------------------------------------------
| The format of a second.
|
| One of
|   * NumericS
|   * TwoDigitS
-}
data SecondFormat
  = NumericS
  | TwoDigitS

{-------------------------------------------------------------------------------
| Format of a second, JS FFI.
-}
secondFormatJS ::
  { numeric :: String
  , twoDigit :: String
  }
secondFormatJS = { numeric: "numeric", twoDigit: "2-digit" }

toSecondFormatJS :: SecondFormat -> String
toSecondFormatJS NumericS = secondFormatJS.numeric

toSecondFormatJS TwoDigitS = secondFormatJS.twoDigit

derive instance eqSecondFormat :: Eq SecondFormat

derive instance ordSecondFormat :: Ord SecondFormat

derive instance genericSecondFormat :: Generic SecondFormat _

instance decodeJsonSecondFormat :: DecodeJson SecondFormat where
  decodeJson = genericDecodeJson

instance encodeJsonSecondFormat :: EncodeJson SecondFormat where
  encodeJson = genericEncodeJson

instance showSecondFormat :: Show SecondFormat where
  show = genericShow

{-------------------------------------------------------------------------------
 ATTENTION: 2 is the number of values of `SecondFormat`.
-}
instance arbitrarySecondFormat :: Arbitrary SecondFormat where
  arbitrary = map intToSecondFormat arbitrary
    where
    intToSecondFormat :: Int -> SecondFormat
    intToSecondFormat n
      | n >= 0 = case n `mod` 2 of
        0 -> NumericS
        _ -> TwoDigitS
      | otherwise = intToSecondFormat (-n)

{-------------------------------------------------------------------------------
| The number of digits in the fractional part of the second.
|
| One of
|   * Digits0
|   * Digits1
|   * Digits2
|   * Digits3
-}
data FractionalSecondDigits
  = Digits0
  | Digits1
  | Digits2
  | Digits3

{-------------------------------------------------------------------------------
| The number of digits in the fractional part of a second, JS interop.
-}
fractionalSecondDigitsJS ::
  { digits0 :: Int
  , digits1 :: Int
  , digits2 :: Int
  , digits3 :: Int
  }
fractionalSecondDigitsJS =
  { digits0: 0
  , digits1: 1
  , digits2: 2
  , digits3: 3
  }

toFractionalDigitsJS :: FractionalSecondDigits -> Int
toFractionalDigitsJS Digits0 = fractionalSecondDigitsJS.digits0

toFractionalDigitsJS Digits1 = fractionalSecondDigitsJS.digits1

toFractionalDigitsJS Digits2 = fractionalSecondDigitsJS.digits2

toFractionalDigitsJS Digits3 = fractionalSecondDigitsJS.digits3

derive instance eqFractionalSecondDigits :: Eq FractionalSecondDigits

derive instance ordFractionalSecondDigits :: Ord FractionalSecondDigits

derive instance genericFractionalSecondDigits :: Generic FractionalSecondDigits _

instance decodeJsonFractionalSecondDigits :: DecodeJson FractionalSecondDigits where
  decodeJson = genericDecodeJson

instance encodeJsonFractionalSecondDigits :: EncodeJson FractionalSecondDigits where
  encodeJson = genericEncodeJson

instance showFractionalSecondDigits :: Show FractionalSecondDigits where
  show = genericShow

{-------------------------------------------------------------------------------
 ATTENTION: 4 is the number of values of `FractionalSecondDigits`.
-}
instance arbitraryFractionalSecondDigits :: Arbitrary FractionalSecondDigits where
  arbitrary = map intToFractionalSecondDigits arbitrary
    where
    intToFractionalSecondDigits :: Int -> FractionalSecondDigits
    intToFractionalSecondDigits n
      | n >= 0 = case n `mod` 4 of
        0 -> Digits0
        0 -> Digits1
        0 -> Digits2
        _ -> Digits3
      | otherwise = intToFractionalSecondDigits (-n)

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

toTimeZoneNameStyleJS :: TimeZoneNameStyle -> String
toTimeZoneNameStyleJS ShortTZ = timeZoneNameStyleJS.short

toTimeZoneNameStyleJS LongTZ = timeZoneNameStyleJS.long

toTimeZoneNameStyleJS ShortOffsetTZ = timeZoneNameStyleJS.shortOffset

toTimeZoneNameStyleJS LongOffsetTZ = timeZoneNameStyleJS.longOffset

toTimeZoneNameStyleJS ShortGenericTZ = timeZoneNameStyleJS.shortGeneric

toTimeZoneNameStyleJS LongGenericTZ = timeZoneNameStyleJS.longGeneric

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
 ATTENTION: 6 is the number of values of `TimeZoneNameStyle`.
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
| `DateTimeFormatOptionsJS` object.
-}
convertDateTimeOptions :: DateTimeFormatOptions -> DateTimeFormatOptionsJS
convertDateTimeOptions (DateTimeFormatOptions options) =
  setDTFOJSFromMaybe setDateStyleJS options.dateStyle (cast {})
    # setDTFOJSFromMaybe setTimeStyleJS options.timeStyle
    # setDTFOJSFromMaybe setCalendarFormatJS options.calendar
    # setDTFOJSFromMaybe setDayPeriodStyleJS options.dayPeriod
    # setDTFOJSFromMaybe setNumberingSystemJS options.numberingSystem
    # setDTFOJSFromMaybe setLocaleMatcherJS options.localeMatcher
    # setDTFOJSFromMaybe setTimeZoneJS options.timeZone
    # setDTFOJSFromMaybe setHour12JS options.hour12
    # setDTFOJSFromMaybe setHourCycleJS options.hourCycle
    # setDTFOJSFromMaybe setFormatMatcherJS options.formatMatcher
    # setDTFOJSFromMaybe setWeekDayFormatJS options.weekDay
    # setDTFOJSFromMaybe setEraFormatJS options.era
    # setDTFOJSFromMaybe setYearFormatJS options.year
    # setDTFOJSFromMaybe setMonthFormatJS options.month
    # setDTFOJSFromMaybe setDayFormatJS options.day
    # setDTFOJSFromMaybe setHourFormatJS options.hour
    # setDTFOJSFromMaybe setMinuteFormatJS options.minute
    # setDTFOJSFromMaybe setSecondFormatJS options.second
    # setDTFOJSFromMaybe setFractionalSecondDigitsJS options.fractionalSecondDigits
    # setDTFOJSFromMaybe setTimeZoneNameStyleJS options.timeZoneNameStyle

setDTFOJSFromMaybe ::
  forall a.
  (DateTimeFormatOptionsJS -> a -> DateTimeFormatOptionsJS) ->
  Maybe a ->
  DateTimeFormatOptionsJS ->
  DateTimeFormatOptionsJS
setDTFOJSFromMaybe f (Just x) fmt = f fmt x

setDTFOJSFromMaybe _ Nothing fmt = fmt

setDateStyleJS ::
  DateTimeFormatOptionsJS ->
  DateStyle -> DateTimeFormatOptionsJS
setDateStyleJS fmt style = cast fmt { dateStyle = toDateStyleJS style }

setTimeStyleJS ::
  DateTimeFormatOptionsJS ->
  TimeStyle -> DateTimeFormatOptionsJS
setTimeStyleJS fmt style = cast fmt { timeStyle = toTimeStyleJS style }

setCalendarFormatJS ::
  DateTimeFormatOptionsJS ->
  CalendarFormat -> DateTimeFormatOptionsJS
setCalendarFormatJS fmt calFmt = cast fmt { calendar = calendarFormatToString calFmt }

setDayPeriodStyleJS ::
  DateTimeFormatOptionsJS ->
  DayPeriod -> DateTimeFormatOptionsJS
setDayPeriodStyleJS fmt style = cast fmt { dayPeriod = toDayPeriodJS style }

setNumberingSystemJS ::
  DateTimeFormatOptionsJS ->
  NumberingSystem -> DateTimeFormatOptionsJS
setNumberingSystemJS fmt numSys = cast fmt { numberingSystem = numberingSystemToString numSys }

setLocaleMatcherJS ::
  DateTimeFormatOptionsJS ->
  LocaleMatcher -> DateTimeFormatOptionsJS
setLocaleMatcherJS fmt matcher = cast fmt { localeMatcher = toLocalMatcherJS matcher }

setTimeZoneJS ::
  DateTimeFormatOptionsJS ->
  TimeZone -> DateTimeFormatOptionsJS
setTimeZoneJS fmt timezone = cast fmt { timeZone = timeZoneToString timezone }

setHour12JS ::
  DateTimeFormatOptionsJS ->
  Hour12 -> DateTimeFormatOptionsJS
setHour12JS fmt hour12 = cast fmt { hour12 = hour12ToBoolean hour12 }

setHourCycleJS ::
  DateTimeFormatOptionsJS ->
  HourCycle -> DateTimeFormatOptionsJS
setHourCycleJS fmt style = cast fmt { hourCycle = toHourCycleJS style }

setFormatMatcherJS ::
  DateTimeFormatOptionsJS ->
  FormatMatcher -> DateTimeFormatOptionsJS
setFormatMatcherJS fmt style = cast fmt { formatMatcher = toFormatMatcherJS style }

setWeekDayFormatJS ::
  DateTimeFormatOptionsJS ->
  WeekDayFormat -> DateTimeFormatOptionsJS
setWeekDayFormatJS fmt style = cast fmt { weekDay = toWeekDayFormatJS style }

setEraFormatJS ::
  DateTimeFormatOptionsJS ->
  EraFormat -> DateTimeFormatOptionsJS
setEraFormatJS fmt style = cast fmt { era = toEraFormatJS style }

setYearFormatJS ::
  DateTimeFormatOptionsJS ->
  YearFormat -> DateTimeFormatOptionsJS
setYearFormatJS fmt style = cast fmt { year = toYearFormatJS style }

setMonthFormatJS ::
  DateTimeFormatOptionsJS ->
  MonthFormat -> DateTimeFormatOptionsJS
setMonthFormatJS fmt style = cast fmt { month = toMonthFormatJS style }

setDayFormatJS ::
  DateTimeFormatOptionsJS ->
  DayFormat -> DateTimeFormatOptionsJS
setDayFormatJS fmt style = cast fmt { day = toDayFormatJS style }

setHourFormatJS ::
  DateTimeFormatOptionsJS ->
  HourFormat -> DateTimeFormatOptionsJS
setHourFormatJS fmt style = cast fmt { hour = toHourFormatJS style }

setMinuteFormatJS ::
  DateTimeFormatOptionsJS ->
  MinuteFormat -> DateTimeFormatOptionsJS
setMinuteFormatJS fmt style = cast fmt { minute = toMinuteFormatJS style }

setSecondFormatJS ::
  DateTimeFormatOptionsJS ->
  SecondFormat -> DateTimeFormatOptionsJS
setSecondFormatJS fmt style = cast fmt { second = toSecondFormatJS style }

setFractionalSecondDigitsJS ::
  DateTimeFormatOptionsJS ->
  FractionalSecondDigits -> DateTimeFormatOptionsJS
setFractionalSecondDigitsJS fmt style = cast fmt { fractionalSecondDigits = toFractionalDigitsJS style }

setTimeZoneNameStyleJS ::
  DateTimeFormatOptionsJS ->
  TimeZoneNameStyle -> DateTimeFormatOptionsJS
setTimeZoneNameStyleJS fmt style = cast fmt { timeZoneNameStyle = toTimeZoneNameStyleJS style }

{-------------------------------------------------------------------------------
| Regex matching a `String` not containing only whitespace or being empty.
-}
notOnlyWhitespaceRegex :: Regex
notOnlyWhitespaceRegex = unsafeRegex "\\S+" unicode
