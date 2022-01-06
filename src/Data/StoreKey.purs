-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2022 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     StoreKey.purs
-- Date:     03.Jan.2022
--
-- ==============================================================================
-- | Module Data.StoreKey, the type class of a key to use for an object when
-- | serializing or deserializing and types needed for this purpose.
module Data.StoreKey
  ( StoreKeyId(..)
  , class StoreKey
  , key
  , storeKeyIdFromObject
  , storeKeyIdToString
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Test.QuickCheck (class Arbitrary)

{-------------------------------------------------------------------------------
| Data type of a key for the local storage.
|
| To get and retrieve objects from the browser's local storage.
-}
newtype StoreKeyId
  = StoreKeyId String

derive newtype instance eqStoreKeyId :: Eq StoreKeyId

derive newtype instance ordStoreKeyId :: Ord StoreKeyId

derive newtype instance showStoreKeyId :: Show StoreKeyId

derive newtype instance decodeJsonStoreKeyId :: DecodeJson StoreKeyId

derive newtype instance encodeJsonStoreKeyId :: EncodeJson StoreKeyId

derive instance genericStoreKeyId :: Generic StoreKeyId _

derive instance newtypeStoreKeyId :: Newtype StoreKeyId _

derive newtype instance arbitraryStoreKeyId :: Arbitrary StoreKeyId

{-------------------------------------------------------------------------------
| Convert a `StoreKeyId` to a String to use it with the local storage functions.
|
| * `key` - The `StoreKeyId` to convert to a String.
-}
storeKeyIdToString :: StoreKeyId -> String
storeKeyIdToString = unwrap

storeKeyIdFromObject :: forall a. StoreKey a => a -> String
storeKeyIdFromObject = storeKeyIdToString <<< key

{-------------------------------------------------------------------------------
| The type class of a key to use to save and load object from local storage.
|
| `key` returns the key for the local storage (a wrapped String) of the object.
-}
class StoreKey a where
  key :: a -> StoreKeyId
