-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2022 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     NumberingSystem.purs
-- Date:     23.Jan.2022
--
-- ==============================================================================
-- | Module Data.NumberingSystem, the type of the numbering system of a locale.
-- | See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl/Locale/numberingSystem
module Data.NumberingSystem
  ( NumberingSystem(..)
  , numberingSystemToString
  ) where

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
|    * Adlm - Adlam digits
|    * Ahom - Ahom digits
|    * Arab - Arabic-Indic digits
|    * Arabext - Extended Arabic-Indic digits
|    * Armn - Armenian upper case numerals — algorithmic
|    * Armnlow - Armenian lower case numerals — algorithmic
|    * Bali - Balinese digits
|    * Beng - Bengali digits
|    * Bhks - Bhaiksuki digits
|    * Brah - Brahmi digits
|    * Cakm - Chakma digits
|    * Cham - Cham digits
|    * Cyrl - Cyrillic numerals — algorithmic
|    * Deva - Devanagari digits
|    * Ethi - Ethiopic numerals — algorithmic
|    * Finance - Financial numerals — may be algorithmic
|    * Fullwide - Full width digits
|    * Geor - Georgian numerals — algorithmic
|    * Gong - Gunjala Gondi digits
|    * Gonm - Masaram Gondi digits
|    * Grek - Greek upper case numerals — algorithmic
|    * Greklow - Greek lower case numerals — algorithmic
|    * Gujr - Gujarati digits
|    * Guru - Gurmukhi digits
|    * Hanidays - Han-character day-of-month numbering for lunar/other traditional calendars
|    * Hanidec - Positional decimal system using Chinese number ideographs as digits
|    * Hans - Simplified Chinese numerals — algorithmic
|    * Hansfin - Simplified Chinese financial numerals — algorithmic
|    * Hant - Traditional Chinese numerals — algorithmic
|    * Hantfin - Traditional Chinese financial numerals — algorithmic
|    * Hebr - Hebrew numerals — algorithmic
|    * Hmng - Pahawh Hmong digits
|    * Hmnp - Nyiakeng Puachue Hmong digits
|    * Java - Javanese digits
|    * Jpan - Japanese numerals — algorithmic
|    * Jpanfin - Japanese financial numerals — algorithmic
|    * Jpanyear - Japanese first-year Gannen numbering for Japanese calendar
|    * Kali - Kayah Li digits
|    * Khmr - Khmer digits
|    * Knda - Kannada digits
|    * Lana - Tai Tham Hora (secular) digits
|    * Lanatham - Tai Tham (ecclesiastical) digits
|    * Laoo - Lao digits
|    * Latn - Latin digits
|    * Lepc - Lepcha digits
|    * Limb - Limbu digits
|    * Mathbold - Mathematical bold digits
|    * Mathdbl - Mathematical double-struck digits
|    * Mathmono - Mathematical monospace digits
|    * Mathsanb - Mathematical sans-serif bold digits
|    * Mathsans - Mathematical sans-serif digits
|    * Mlym - Malayalam digits
|    * Modi - Modi digits
|    * Mong - Mongolian digits
|    * Mroo - Mro digits
|    * Mtei - Meetei Mayek digits
|    * Mymr - Myanmar digits
|    * Mymrshan - Myanmar Shan digits
|    * Mymrtlng - Myanmar Tai Laing digits
|    * Native - Native digits
|    * Newa - Newa digits
|    * Nkoo - N'Ko digits
|    * Olck - Ol Chiki digits
|    * Orya - Oriya digits
|    * Osma - Osmanya digits
|    * Rohg - Hanifi Rohingya digits
|    * Roman - Roman upper case numerals — algorithmic
|    * Romanlow - Roman lowercase numerals — algorithmic
|    * Saur - Saurashtra digits
|    * Shrd - Sharada digits
|    * Sind - Khudawadi digits
|    * Sinh - Sinhala Lith digits
|    * Sora - Sora_Sompeng digits
|    * Sund - Sundanese digits
|    * Takr - Takri digits
|    * Talu - New Tai Lue digits
|    * Taml - Tamil numerals — algorithmic
|    * Tamldec - Modern Tamil decimal digits
|    * Telu - Telugu digits
|    * Thai - Thai digits
|    * Tirh - Tirhuta digits
|    * Tibt - Tibetan digits
|    * Traditio - Traditional numerals — may be algorithmic
|    * Vaii - Vai digits
|    * Wara - Warang Citi digits
|    * Wcho - Wancho digits
-}
data NumberingSystem
  = Adlm
  | Ahom
  | Arab
  | Arabext
  | Armn
  | Armnlow
  | Bali
  | Beng
  | Bhks
  | Brah
  | Cakm
  | Cham
  | Cyrl
  | Deva
  | Ethi
  | Finance
  | Fullwide
  | Geor
  | Gong
  | Gonm
  | Grek
  | Greklow
  | Gujr
  | Guru
  | Hanidays
  | Hanidec
  | Hans
  | Hansfin
  | Hant
  | Hantfin
  | Hebr
  | Hmng
  | Hmnp
  | Java
  | Jpan
  | Jpanfin
  | Jpanyear
  | Kali
  | Khmr
  | Knda
  | Lana
  | Lanatham
  | Laoo
  | Latn
  | Lepc
  | Limb
  | Mathbold
  | Mathdbl
  | Mathmono
  | Mathsanb
  | Mathsans
  | Mlym
  | Modi
  | Mong
  | Mroo
  | Mtei
  | Mymr
  | Mymrshan
  | Mymrtlng
  | Native
  | Newa
  | Nkoo
  | Olck
  | Orya
  | Osma
  | Rohg
  | Roman
  | Romanlow
  | Saur
  | Shrd
  | Sind
  | Sinh
  | Sora
  | Sund
  | Takr
  | Talu
  | Taml
  | Tamldec
  | Telu
  | Thai
  | Tirh
  | Tibt
  | Traditio
  | Vaii
  | Wara
  | Wcho

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
        0 -> Adlm
        1 -> Ahom
        2 -> Arab
        3 -> Arabext
        4 -> Armn
        5 -> Armnlow
        6 -> Bali
        7 -> Beng
        8 -> Bhks
        9 -> Brah
        10 -> Cakm
        11 -> Cham
        12 -> Cyrl
        13 -> Deva
        14 -> Ethi
        15 -> Finance
        16 -> Fullwide
        17 -> Geor
        18 -> Gong
        19 -> Gonm
        20 -> Grek
        21 -> Greklow
        22 -> Gujr
        23 -> Guru
        24 -> Hanidays
        25 -> Hanidec
        26 -> Hans
        27 -> Hansfin
        28 -> Hant
        29 -> Hantfin
        30 -> Hebr
        31 -> Hmng
        32 -> Hmnp
        33 -> Java
        34 -> Jpan
        35 -> Jpanfin
        36 -> Jpanyear
        37 -> Kali
        38 -> Khmr
        39 -> Knda
        40 -> Lana
        41 -> Lanatham
        42 -> Laoo
        43 -> Latn
        44 -> Lepc
        45 -> Limb
        46 -> Mathbold
        47 -> Mathdbl
        48 -> Mathmono
        49 -> Mathsanb
        50 -> Mathsans
        51 -> Mlym
        52 -> Modi
        53 -> Mong
        54 -> Mroo
        55 -> Mtei
        56 -> Mymr
        57 -> Mymrshan
        58 -> Mymrtlng
        59 -> Native
        60 -> Newa
        61 -> Nkoo
        62 -> Olck
        63 -> Orya
        64 -> Osma
        65 -> Rohg
        66 -> Roman
        67 -> Romanlow
        68 -> Saur
        69 -> Shrd
        70 -> Sind
        71 -> Sinh
        72 -> Sora
        73 -> Sund
        74 -> Takr
        75 -> Talu
        76 -> Taml
        77 -> Tamldec
        78 -> Telu
        79 -> Thai
        80 -> Tirh
        81 -> Tibt
        82 -> Traditio
        83 -> Vaii
        84 -> Wara
        _ -> Wcho
      | otherwise = intToNumberingSystem (-n)

{-------------------------------------------------------------------------------
| Return the string representation of the `NumberingSystem`.
-}
numberingSystemToString :: NumberingSystem -> String
numberingSystemToString Adlm = "adlm"

numberingSystemToString Ahom = "ahom"

numberingSystemToString Arab = "arab"

numberingSystemToString Arabext = "arabext"

numberingSystemToString Armn = "armn"

numberingSystemToString Armnlow = "armnlow"

numberingSystemToString Bali = "bali"

numberingSystemToString Beng = "beng"

numberingSystemToString Bhks = "bhks"

numberingSystemToString Brah = "brah"

numberingSystemToString Cakm = "cakm"

numberingSystemToString Cham = "cham"

numberingSystemToString Cyrl = "cyrl"

numberingSystemToString Deva = "deva"

numberingSystemToString Ethi = "ethi"

numberingSystemToString Finance = "finance"

numberingSystemToString Fullwide = "fullwide"

numberingSystemToString Geor = "geor"

numberingSystemToString Gong = "gong"

numberingSystemToString Gonm = "gonm"

numberingSystemToString Grek = "grek"

numberingSystemToString Greklow = "greklow"

numberingSystemToString Gujr = "gujr"

numberingSystemToString Guru = "guru"

numberingSystemToString Hanidays = "hanidays"

numberingSystemToString Hanidec = "hanidec"

numberingSystemToString Hans = "hans"

numberingSystemToString Hansfin = "hansfin"

numberingSystemToString Hant = "hant"

numberingSystemToString Hantfin = "hantfin"

numberingSystemToString Hebr = "hebr"

numberingSystemToString Hmng = "hmng"

numberingSystemToString Hmnp = "hmnp"

numberingSystemToString Java = "java"

numberingSystemToString Jpan = "jpan"

numberingSystemToString Jpanfin = "jpanfin"

numberingSystemToString Jpanyear = "jpanyear"

numberingSystemToString Kali = "kali"

numberingSystemToString Khmr = "khmr"

numberingSystemToString Knda = "knda"

numberingSystemToString Lana = "lana"

numberingSystemToString Lanatham = "lanatham"

numberingSystemToString Laoo = "laoo"

numberingSystemToString Latn = "latn"

numberingSystemToString Lepc = "lepc"

numberingSystemToString Limb = "limb"

numberingSystemToString Mathbold = "mathbold"

numberingSystemToString Mathdbl = "mathdbl"

numberingSystemToString Mathmono = "mathmono"

numberingSystemToString Mathsanb = "mathsanb"

numberingSystemToString Mathsans = "mathsans"

numberingSystemToString Mlym = "mlym"

numberingSystemToString Modi = "modi"

numberingSystemToString Mong = "mong"

numberingSystemToString Mroo = "mroo"

numberingSystemToString Mtei = "mtei"

numberingSystemToString Mymr = "mymr"

numberingSystemToString Mymrshan = "mymrshan"

numberingSystemToString Mymrtlng = "mymrtlng"

numberingSystemToString Native = "native"

numberingSystemToString Newa = "newa"

numberingSystemToString Nkoo = "nkoo"

numberingSystemToString Olck = "olck"

numberingSystemToString Orya = "orya"

numberingSystemToString Osma = "osma"

numberingSystemToString Rohg = "rohg"

numberingSystemToString Roman = "roman"

numberingSystemToString Romanlow = "romanlow"

numberingSystemToString Saur = "saur"

numberingSystemToString Shrd = "shrd"

numberingSystemToString Sind = "sind"

numberingSystemToString Sinh = "sinh"

numberingSystemToString Sora = "sora"

numberingSystemToString Sund = "sund"

numberingSystemToString Takr = "takr"

numberingSystemToString Talu = "talu"

numberingSystemToString Taml = "taml"

numberingSystemToString Tamldec = "tamldec"

numberingSystemToString Telu = "telu"

numberingSystemToString Thai = "thai"

numberingSystemToString Tirh = "tirh"

numberingSystemToString Tibt = "tibt"

numberingSystemToString Traditio = "traditio"

numberingSystemToString Vaii = "vaii"

numberingSystemToString Wara = "wara"

numberingSystemToString Wcho = "wcho"
