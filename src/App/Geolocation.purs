-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2022 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     Geolocation.purs
-- Date:     26.Jan.2022
--
-- ==============================================================================
-- | Module App.Geolocation, types and functions for the geolocation API.
-- | See https://developer.mozilla.org/en-US/docs/Web/API/Geolocation
-- |
-- | Example, getting the current position:
-- |
-- | ```purescript
-- | do
-- |   posE <- getCurrentPosition defaultGeoLocOptions
-- |   case posE of
-- |     Left err -> liftEffect $ log $ show err
-- |     Right pos -> liftEffect $ log $ show pos
-- | ```
-- |
-- | Example, getting the current position using callbacks:
-- |
-- | ```purescript
-- | do
-- |   getCurrentPositionCB
-- |     defaultGeoLocOptions
-- |     (\pos -> log $ show pos)
-- |     (\err -> log $ show err)
-- | ```
module App.Geolocation
  ( GeolocationPosition(..)
  , PositionError
  , PositionRecordJS
  , WatchId(..)
  , clearWatch
  , getCurrentPosition
  , getCurrentPositionCB
  , module Reexports
  , showLatitudeLongitude
  , supportsGeoLocation
  , watchPositionCB
  ) where

import Prelude
import App.GeoLocOptions (GeoLocOptions, GeoLocOptionsJS)
import App.GeoLocOptions
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
  )
  as Reexports
import Control.Promise (Promise, toAffE)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (Aff, Error, attempt)
import Literals.Null (Null)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)
import Untagged.Union (type (|+|), fromOneOf)

{-------------------------------------------------------------------------------
| Return `true`, if detecting the current position (GPS on mobile, network
| routing, ...) is supported, `false` else.
-}
supportsGeoLocation :: Boolean
supportsGeoLocation = supportsGeoLocationJS unit

foreign import supportsGeoLocationJS :: Unit -> Boolean

{-------------------------------------------------------------------------------
| The type holding data about the current geolocation.
|
| See: https://developer.mozilla.org/en-US/docs/Web/API/GeolocationPosition
|
| Fields:
|
| * `latitude` - The position's latitude in decimal degrees.
| * `longitude` - The position's longitude in decimal degrees.
| * `altitude` - The altitude above sea level in meters. If supported by the
|                device, else this in `Nothing`.
| * `accuracy` - The accuracy in meters of the position (`latitude` and
|                `longitude`)
| * `altitudeAccuracy` - The accuracy of `altitude` in meters. `Nothing`, if
|                        `altitude` is unsupported.
| * `heading` - The direction the device is facing in degrees. 0° is north, 90°
|               is east. If `speed` is 0.0, this is `nan`. If heading
|               information isn't supported by the device, this is `Nothing`.
| * `speed` - The velocity of the device in `m/s` (meters per second). Only
|             present if applicable, else it is `Nothing`.
| * `timestamp` - Milliseconds of the date and time of the position's
|                 generation.
-}
newtype GeolocationPosition
  = GeolocationPosition
  { latitude :: Number
  , longitude :: Number
  , altitude :: Maybe Number
  , accuracy :: Number
  , altitudeAccuracy :: Maybe Number
  , heading :: Maybe Number
  , speed :: Maybe Number
  , timestamp :: Number
  }

derive instance eqGeolocationPosition :: Eq GeolocationPosition

derive instance ordGeolocationPosition :: Ord GeolocationPosition

derive instance genericGeolocationPosition :: Generic GeolocationPosition _

instance decodeJsonGeolocationPosition :: DecodeJson GeolocationPosition where
  decodeJson = genericDecodeJson

instance encodeJsonGeolocationPosition :: EncodeJson GeolocationPosition where
  encodeJson = genericEncodeJson

instance showGeolocationPosition :: Show GeolocationPosition where
  show = genericShow

instance arbitraryGeolocationPosition :: Arbitrary GeolocationPosition where
  arbitrary = genericArbitrary

{-------------------------------------------------------------------------------
| Return a String containing the latitude and longitude of the position.
|
| Example:
|
| (49.0435799°, 19.733561°)
-}
showLatitudeLongitude :: GeolocationPosition -> String
showLatitudeLongitude (GeolocationPosition pos) =
  "("
    <> addDegreeSign pos.latitude
    <> ", "
    <> addDegreeSign pos.longitude
    <> ")"
  where
  addDegreeSign num = show num <> "°"

{-------------------------------------------------------------------------------
| Record holding the error information if a position request has failed.
|
| * `code` - The error's ID
| * `message` - A string with the actual error message.
-}
type PositionError
  = { code :: Int
    , message :: String
    }

{-------------------------------------------------------------------------------
| Request the current position using callbacks.
|
| There is also a version using an `Aff` instead of callbacks:
| `getCurrentPosition`.
|
| * `options` - The `GeoLocOptions` for this request.
| * `successCB` - This callback is called with a filled `GeolocationPosition` on
|                 success
| * `errorCB` - This callback is called with a filled `PositionError` on errors.
-}
getCurrentPositionCB ::
  GeoLocOptions ->
  (GeolocationPosition -> Effect Unit) ->
  (PositionError -> Effect Unit) ->
  Effect Unit
getCurrentPositionCB options successCB errorCB =
  runFn3
    currPositionJS
    (\p -> successCB $ fromPosJS p)
    errorCB
    (unwrap options)

{-------------------------------------------------------------------------------
| Request the current position as an `Aff`.
|
| Return the result in an `Either`.
| In `Left err`, the error string `err` is returned on failure, on success the
| filled `GeolocationPosition` pos is returned as `Right pos`
|
| * `options` - The `GeoLocOptions` of this request.
-}
getCurrentPosition :: GeoLocOptions -> Aff (Either Error GeolocationPosition)
getCurrentPosition options = do
  res <- attempt $ toAffE $ currPositionAffJs $ unwrap options
  case res of
    Left err -> pure $ Left err
    Right posJS -> pure $ Right $ fromPosJS posJS

{-------------------------------------------------------------------------------
| Register a callback to watch for changes in the device's position.
|
| Return a `WatchId`, to cancel the watch using `clearWatch`.
|
| * `options` - The `GeoLocOptions` for this request.
| * `successCB` - This callback is called with a filled `GeolocationPosition` of
|                 the new position on success
| * `errorCB` - This callback is called with a filled `PositionError` on errors.
-}
watchPositionCB ::
  GeoLocOptions ->
  (GeolocationPosition -> Effect Unit) ->
  (PositionError -> Effect Unit) ->
  Effect WatchId
watchPositionCB options successCB errorCB =
  runFn3
    watchPositionJS
    (\p -> successCB $ fromPosJS p)
    errorCB
    (unwrap options)

{-------------------------------------------------------------------------------
| Do not watch changes of the geolocation position with ID `id` any more.
|
| * `id` - The `WatchId` returned from `watchPositionCB`.
-}
clearWatch :: WatchId -> Effect Unit
clearWatch id = clearWatchJS (unwrap id)

foreign import currPositionAffJs ::
  GeoLocOptionsJS -> Effect (Promise PositionRecordJS)

foreign import currPositionJS ::
  Fn3
    (PositionRecordJS -> (Effect Unit))
    (PositionError -> (Effect Unit))
    GeoLocOptionsJS
    (Effect Unit)

foreign import watchPositionJS ::
  Fn3
    (PositionRecordJS -> (Effect Unit))
    (PositionError -> (Effect Unit))
    GeoLocOptionsJS
    (Effect WatchId)

foreign import clearWatchJS :: Int -> Effect Unit

{-------------------------------------------------------------------------------
| The type of a geolocation watch id.
|
| Used to end the watching by passing this to `clearWatch`
-}
newtype WatchId
  = WatchId Int

derive newtype instance eqWatchId :: Eq WatchId

derive newtype instance ordWatchId :: Ord WatchId

derive newtype instance showWatchId :: Show WatchId

derive newtype instance decodeJsonWatchId :: DecodeJson WatchId

derive newtype instance encodeJsonWatchId :: EncodeJson WatchId

derive instance genericWatchId :: Generic WatchId _

derive instance newtypeWatchId :: Newtype WatchId _

derive newtype instance arbitraryWatchId :: Arbitrary WatchId

{-------------------------------------------------------------------------------
| The record for interop with JS.
|
| Some fields can be either `null` or a `Number`.
-}
type PositionRecordJS
  = { latitude :: Number
    , longitude :: Number
    , altitude :: Null |+| Number
    , accuracy :: Number
    , altitudeAccuracy :: Null |+| Number
    , heading :: Null |+| Number
    , speed :: Null |+| Number
    , timestamp :: Number
    }

{-------------------------------------------------------------------------------
| Helper function to convert a given `PositionRecordJS` from the JS FFI to a
| `GeolocationPosition`.
-}
fromPosJS :: PositionRecordJS -> GeolocationPosition
fromPosJS pos =
  GeolocationPosition
    { latitude: pos.latitude
    , longitude: pos.longitude
    , altitude: fromOneOf pos.altitude
    , accuracy: pos.accuracy
    , altitudeAccuracy: fromOneOf pos.altitudeAccuracy
    , heading: fromOneOf pos.heading
    , speed: fromOneOf pos.speed
    , timestamp: pos.timestamp
    }
