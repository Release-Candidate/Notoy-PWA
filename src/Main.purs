-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     Main.purs
-- Date:     23.Dec.2021
--
-- =============================================================================
-- | # Module Main
-- |
-- | Main entry point of the app.
module Main
  ( main
  ) where

import Prelude
import Effect (Effect)
import ShareTarget.Event (registerShareEvent)
import Web.HTML (window)

-- | Main entry point of the app.
main :: Effect Unit
main = window >>= registerShareEvent
