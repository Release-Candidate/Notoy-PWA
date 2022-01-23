-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2022 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     NumberingSystem.purs
-- Date:     23.Jan.2022
--
-- ==============================================================================
-- | Module Data.NumberingSystem, the type of the numbering system of a locale.
module Data.NumberingSystem where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Test.QuickCheck (class Arbitrary, arbitrary)

{-------------------------------------------------------------------------------
 | The numbering systems of locales.
 |
 | See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/DateTimeFormat/DateTimeFormat
 |
 | One of
 |   adlm	Adlam digits
ahom	Ahom digits
arab	Arabic-Indic digits
arabext	Extended Arabic-Indic digits
armn	Armenian upper case numerals — algorithmic
armnlow	Armenian lower case numerals — algorithmic
bali	Balinese digits
beng	Bengali digits
bhks	Bhaiksuki digits
brah	Brahmi digits
cakm	Chakma digits
cham	Cham digits
cyrl	Cyrillic numerals — algorithmic
deva	Devanagari digits
ethi	Ethiopic numerals — algorithmic
finance	Financial numerals — may be algorithmic
fullwide	Full width digits
geor	Georgian numerals — algorithmic
gong	Gunjala Gondi digits
gonm	Masaram Gondi digits
grek	Greek upper case numerals — algorithmic
greklow	Greek lower case numerals — algorithmic
gujr	Gujarati digits
guru	Gurmukhi digits
hanidays	Han-character day-of-month numbering for lunar/other traditional calendars
hanidec	Positional decimal system using Chinese number ideographs as digits
hans	Simplified Chinese numerals — algorithmic
hansfin	Simplified Chinese financial numerals — algorithmic
hant	Traditional Chinese numerals — algorithmic
hantfin	Traditional Chinese financial numerals — algorithmic
hebr	Hebrew numerals — algorithmic
hmng	Pahawh Hmong digits
hmnp	Nyiakeng Puachue Hmong digits
java	Javanese digits
jpan	Japanese numerals — algorithmic
jpanfin	Japanese financial numerals — algorithmic
jpanyear	Japanese first-year Gannen numbering for Japanese calendar
kali	Kayah Li digits
khmr	Khmer digits
knda	Kannada digits
lana	Tai Tham Hora (secular) digits
lanatham	Tai Tham (ecclesiastical) digits
laoo	Lao digits
latn	Latin digits
lepc	Lepcha digits
limb	Limbu digits
mathbold	Mathematical bold digits
mathdbl	Mathematical double-struck digits
mathmono	Mathematical monospace digits
mathsanb	Mathematical sans-serif bold digits
mathsans	Mathematical sans-serif digits
mlym	Malayalam digits
modi	Modi digits
mong	Mongolian digits
mroo	Mro digits
mtei	Meetei Mayek digits
mymr	Myanmar digits
mymrshan	Myanmar Shan digits
mymrtlng	Myanmar Tai Laing digits
native	Native digits
newa	Newa digits
nkoo	N'Ko digits
olck	Ol Chiki digits
orya	Oriya digits
osma	Osmanya digits
rohg	Hanifi Rohingya digits
roman	Roman upper case numerals — algorithmic
romanlow	Roman lowercase numerals — algorithmic
saur	Saurashtra digits
shrd	Sharada digits
sind	Khudawadi digits
sinh	Sinhala Lith digits
sora	Sora_Sompeng digits
sund	Sundanese digits
takr	Takri digits
talu	New Tai Lue digits
taml	Tamil numerals — algorithmic
tamldec	Modern Tamil decimal digits
telu	Telugu digits
thai	Thai digits
tirh	Tirhuta digits
tibt	Tibetan digits
traditio	Traditional numerals — may be algorithmic
vaii	Vai digits
wara	Warang Citi digits
wcho	Wancho digits
 -}
data NumberingSystem
  = A
  | B

derive instance eqNumberingSystem :: Eq NumberingSystem

derive instance ordNumberingSystem :: Ord NumberingSystem

derive instance genericNumberingSystem :: Generic NumberingSystem _

instance decodeJsonNumberingSystem :: DecodeJson NumberingSystem where
  decodeJson = genericDecodeJson

instance encodeJsonNumberingSystem :: EncodeJson NumberingSystem where
  encodeJson = genericEncodeJson

instance showNumberingSystem :: Show NumberingSystem where
  show = genericShow

{-------------------------------------------------------------------------------
 | ATTENTION: 86 is the number of values of `NumberingSystem`.
 -}
instance arbitraryNumberingSystem :: Arbitrary NumberingSystem where
  arbitrary = map intToNumberingSystem arbitrary
    where
    intToNumberingSystem :: Int -> NumberingSystem
    intToNumberingSystem n
      | n >= 0 = case n `mod` 86 of
        0 -> A
        _ -> B
      | otherwise = intToNumberingSystem (-n)
