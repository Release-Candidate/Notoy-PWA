-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     Note.purs
-- Date:     23.Dec.2021
--
-- ==============================================================================
-- | Module Data.Note, module holding functions and records about the note data.
module Data.Note
  ( Note(..)
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Options (Options)
import Data.Show.Generic (genericShow)

data Note
  = Note
    { title :: Maybe String
    , url :: Maybe String
    , shortDesc :: Maybe String
    , longDesc :: Maybe String
    , options :: Options
    }

derive instance eqNote :: Eq Note

derive instance genericNote :: Generic Note _

instance showNote :: Show Note where
  show = genericShow
