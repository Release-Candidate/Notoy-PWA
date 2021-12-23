-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     Options.purs
-- Date:     23.Dec.2021
--
-- ==============================================================================
-- | Module Data.Options, the record holding the app's options and related
-- | functions.
module Data.Options
  ( Format(..)
  , Options(..)
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Format
  = Markdown
  | OrgMode
  | Text

derive instance eqFormat :: Eq Format

derive instance genericFormat :: Generic Format _

instance showFormat :: Show Format where
  show = genericShow

data Options
  = Options
    { format :: Format
    , addDate :: Boolean
    , addYaml :: Boolean
    }

derive instance eqOptions :: Eq Options

derive instance genericOptions :: Generic Options _

instance showOptions :: Show Options where
  show = genericShow
