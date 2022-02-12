-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2022 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     GeoLocOptions.purs
-- Date:     27.Jan.2022
--
-- ==============================================================================
-- | Module App.GeoLocOptions, the options to set for geolocation functions.
-- | See https://developer.mozilla.org/en-US/docs/Web/API/Geolocation/getCurrentPosition
module App.GeoLocOptions
  ( GeoLocOptions(..)
  , GeoLocOptionsJS
  , HighAccuracy(..)
  , defaultGeoLocOptions
  , setAccuracy
  , setAlwaysUseCached
  , setMaxCacheAge
  , setNeverUseCached
  , setNoTimeout
  , setTimeout
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.Number (infinity)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Milliseconds(..))
import Test.QuickCheck (class Arbitrary, arbitrary)

{-------------------------------------------------------------------------------
| The options to set to get the current geolocation.
|
| Do not set the values directly, use the `set...` functions.
|
| Example:
|
| ```purescript
| geoLocOpts :: Maybe GeoLocOptions
| geoLocOpts =
|   defaultGeoLocOptions
|     # setAccuracy HighAccuracy
|     # setMaxCacheAge (Milliseconds 5000.0)
|     >>= (setTimeout (Milliseconds 100.0))
| ```
|
| * `enableHighAccuracy`  - If this is `true`, try to get positions with higher
|                           accuracy (valid for GPS). Default: `false`
| * `timeout` - The timeout in milliseconds to wait for a geolocation value.
|               `infinity` means there is no timeout (`0` means???).
|               Default: `infinity`.
| * `maximumAge` - The maximum age of a cached position in milliseconds, that is
|                  used as the current position. `0.0` always requests a new
|                  position, `infinity` always uses the cached position.
|                  Default: `0.0`.
-}
newtype GeoLocOptions
  = GeoLocOptions GeoLocOptionsJS

derive newtype instance eqGeoLocOptions :: Eq GeoLocOptions

derive newtype instance ordGeoLocOptions :: Ord GeoLocOptions

derive newtype instance showGeoLocOptions :: Show GeoLocOptions

derive newtype instance decodeJsonGeoLocOptions :: DecodeJson GeoLocOptions

derive newtype instance encodeJsonGeoLocOptions :: EncodeJson GeoLocOptions

derive instance genericGeoLocOptions :: Generic GeoLocOptions _

derive instance newtypeGeoLocOptions :: Newtype GeoLocOptions _

derive newtype instance arbitraryGeoLocOptions :: Arbitrary GeoLocOptions

{-------------------------------------------------------------------------------
| The default values for a `GeoLocOptions`.
|
| * Use normal accuracy when setting the current position.
| * Do not use a timeout, wait forever for an answer.
| * Do not use cached values, always request a new position.
-}
defaultGeoLocOptions :: GeoLocOptions
defaultGeoLocOptions =
  GeoLocOptions
    { enableHighAccuracy: false
    , timeout: infinity
    , maximumAge: 0.0
    }

{-------------------------------------------------------------------------------
| Set the accuracy of the geolocation acquisition.
|
| The default is `NormalAccuracy`.
-}
setAccuracy :: HighAccuracy -> GeoLocOptions -> GeoLocOptions
setAccuracy HighAccuracy (GeoLocOptions options) = wrap options { enableHighAccuracy = true }

setAccuracy NormalAccuracy (GeoLocOptions options) = wrap options { enableHighAccuracy = false }

{-------------------------------------------------------------------------------
| Disable the timeout for geolocation queries, wait forever for an answer.
|
| This is the default.
-}
setNoTimeout :: GeoLocOptions -> GeoLocOptions
setNoTimeout (GeoLocOptions options) = wrap options { timeout = infinity }

{-------------------------------------------------------------------------------
| Set the timeout in milliseconds, only non-negative values are allowed.
|
| What does a timeout of `0.0 ms` do?
-}
setTimeout :: Milliseconds -> GeoLocOptions -> Maybe GeoLocOptions
setTimeout (Milliseconds timeout) (GeoLocOptions options)
  | timeout < 0.0 = Nothing
  | otherwise = Just $ wrap options { timeout = timeout }

{-------------------------------------------------------------------------------
| Set the `GeoLocOptions` to always use the cached position, never request a
| new one.
-}
setAlwaysUseCached :: GeoLocOptions -> GeoLocOptions
setAlwaysUseCached (GeoLocOptions options) = wrap options { maximumAge = infinity }

{-------------------------------------------------------------------------------
| Set the `GeoLocOptions` to never use the cached position, always request a
| new one.
|
| This is the default.
-}
setNeverUseCached :: GeoLocOptions -> GeoLocOptions
setNeverUseCached (GeoLocOptions options) = wrap options { maximumAge = 0.0 }

{-------------------------------------------------------------------------------
| Set the maximum age of the cached position in milliseconds.
|
| If the cached value is older than this, a new position is requested. Negative
| values are not allowed, `0.0` never uses a cached position and always requests
| a new one.
-}
setMaxCacheAge :: Milliseconds -> GeoLocOptions -> Maybe GeoLocOptions
setMaxCacheAge (Milliseconds maxAge) (GeoLocOptions options)
  | maxAge < 0.0 = Nothing
  | otherwise = Just $ wrap options { maximumAge = maxAge }

{-------------------------------------------------------------------------------
| Use high accuracy position calculation or not.
|
| One of
|   * HighAccuracy
|   * NormalAccuracy
-}
data HighAccuracy
  = HighAccuracy
  | NormalAccuracy

derive instance eqHighAccuracy :: Eq HighAccuracy

derive instance ordHighAccuracy :: Ord HighAccuracy

derive instance genericHighAccuracy :: Generic HighAccuracy _

instance decodeJsonHighAccuracy :: DecodeJson HighAccuracy where
  decodeJson = genericDecodeJson

instance encodeJsonHighAccuracy :: EncodeJson HighAccuracy where
  encodeJson = genericEncodeJson

instance showHighAccuracy :: Show HighAccuracy where
  show = genericShow

{-------------------------------------------------------------------------------
 ATTENTION: 2 is the number of values of `HighAccuracy`.
-}
instance arbitraryHighAccuracy :: Arbitrary HighAccuracy where
  arbitrary = map intToHighAccuracy arbitrary
    where
    intToHighAccuracy :: Int -> HighAccuracy
    intToHighAccuracy n
      | n >= 0 = case n `mod` 2 of
        0 -> HighAccuracy
        _ -> NormalAccuracy
      | otherwise = intToHighAccuracy (-n)

{-------------------------------------------------------------------------------
| The options to set to get the current geolocation.
|
| For JS interop only.
|
| * `enableHighAccuracy`  - If this is `true`, try to get positions with higher
|                           accuracy (valid for GPS). Default: `false`
| * `timeout` - The timeout in milliseconds to wait for a geolocation value.
|               `infinity` means there is no timeout (`0` means???).
|               Default: `infinity`.
| * `maximumAge` - The maximum age of a cached position in milliseconds, that is
|                  used as the current position. `0.0` always requests a new
|                  position, `infinity` always uses the cached position.
|                  Default: `0.0`.
-}
type GeoLocOptionsJS
  = { enableHighAccuracy :: Boolean
    , timeout :: Number
    , maximumAge :: Number
    }
