-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     General.purs
-- Date:     23.Dec.2021
--
-- ==============================================================================
-- | Module Helpers.General, contains general helper functions.
module Helpers.General
  ( showM
  ) where

import Prelude
import Data.Maybe (Maybe, fromMaybe)

{-
| Return an empty String `""`, if the parameter is `Nothing`, the string else.
|
| * s :: Maybe String - if this is `Nothing`, the empty `String` is returned.
|                       Else the string is returned.
-}
showM :: Maybe String -> String
showM = show <<< fromMaybe ""
