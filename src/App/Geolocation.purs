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
module App.Geolocation
  ( WatchId(..)
  , clearWatch
  , currPositionJS
  , module Reexports
  , supportsGeoLocation
  ) where

import Prelude
import App.GeoLocOptions (GeoLocOptions(..), GeoLocOptionsJS, HighAccuracy(..), defaultGeoLocOptions, setAccuracy, setAlwaysUseCached, setMaxCacheAge, setNeverUseCached, setNoTimeout, setTimeout) as Reexports
import App.GeoLocOptions (GeoLocOptionsJS)
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Function.Uncurried (Fn2, Fn3, Fn4)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
import Test.QuickCheck (class Arbitrary)

{-------------------------------------------------------------------------------
| Return `true`, if detecting the current position (GPS on mobile, network
| routing, ...) is supported, `false` else.
-}
supportsGeoLocation :: Boolean
supportsGeoLocation = supportsGeoLocationJS unit

foreign import supportsGeoLocationJS :: Unit -> Boolean

foreign import currPositionJS ::
  Fn3
    (Fn4 Number Number Number Int (Effect Unit))
    (Fn2 Int String (Effect Unit))
    GeoLocOptionsJS
    (Effect Unit)

{-------------------------------------------------------------------------------
| Do not watch changes of the geolocation position with ID `id` any more.
-}
clearWatch :: WatchId -> Effect Unit
clearWatch id = clearWatchJS (unwrap id)

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
