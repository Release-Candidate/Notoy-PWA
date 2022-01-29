-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2022 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     BigDataGeoLoc.purs
-- Date:     29.Jan.2022
--
-- ==============================================================================
-- | Module App.BigDataGeoLoc, to use BigData for reverse geolocation.
-- | See: https://www.bigdatacloud.com/geocoding-apis/free-reverse-geocode-to-city-api
module App.BigDataGeoLoc
  ( bigDataGeolocResponse
  , bigDataGeolocURL
  ) where

import Prelude
import App.Geolocation (GeolocationPosition(..))
import Data.Argonaut (class DecodeJson, Json, decodeJson, (.!=), (.:), (.:?))
import Data.DateTimeFormat (Locale, localeToString)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)

{-------------------------------------------------------------------------------
| Return an URL to request the location of the given `GeolocationPosition`.
|
| * `locale` - The locale to request the location name in.
| + `pos` - The position as latitude and longitude.
-}
bigDataGeolocURL :: Locale -> GeolocationPosition -> String
bigDataGeolocURL locale (GeolocationPosition pos) =
  "https://api.bigdatacloud.net/data/reverse-geocode-client?latitude="
    <> show pos.latitude
    <> "&longitude="
    <> show pos.longitude
    <> "&localityLanguage="
    <> localeToString locale

{-------------------------------------------------------------------------------
| Convert the JSON response from the BigData reverse geolocation request to a
| string.
-}
bigDataGeolocResponse :: Json -> String
bigDataGeolocResponse json = case decodeJson json of
  Left _ -> ""
  Right (dat :: BigDataJson) -> show dat

{-------------------------------------------------------------------------------
| Helper to gather fields from the JSON answer that are of interest.
-}
newtype BigDataJson
  = BigDataJson
  { latitude :: Number
  , longitude :: Number
  , continent :: String
  , city :: String
  , countryName :: String
  , locality :: String
  }

derive instance eqBigDataJson :: Eq BigDataJson

derive instance ordBigDataJson :: Ord BigDataJson

derive instance genericBigDataJson :: Generic BigDataJson _

instance showBigDataJson :: Show BigDataJson where
  show (BigDataJson dat) =
    continent' <> countryName' <> city' <> dat.locality
      <> " ("
      <> addDegreeSign dat.latitude
      <> ", "
      <> addDegreeSign dat.longitude
      <> ")"
    where
    addHyphenOrNot string = case string of
      "" -> ""
      st -> st <> " - "

    addDegreeSign num = show num <> "°"

    continent' = addHyphenOrNot dat.continent

    countryName' = addHyphenOrNot dat.countryName

    city' = addHyphenOrNot dat.city

instance arbitraryBigDataJson :: Arbitrary BigDataJson where
  arbitrary = genericArbitrary

instance decodeJsonBigDataJson :: DecodeJson BigDataJson where
  decodeJson json = do
    x <- decodeJson json
    latitude <- x .: "latitude"
    longitude <- x .: "longitude"
    continent <- x .:? "continent" .!= ""
    city <- x .:? "city" .!= ""
    countryName <- x .:? "countryName" .!= ""
    locality <- x .:? "locality" .!= ""
    pure
      $ BigDataJson
          { latitude
          , longitude
          , continent
          , city
          , countryName
          , locality
          }
