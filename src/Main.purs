-- SPDX-License-Identifier: GPL-3.0-or-later
-- Copyright (C) 2021 Roland Csaszar
--
-- Project:  notoy-pwa
-- File:     Main.purs
-- Date:     23.Dec.2021
--
-- ==============================================================================
module Main
  ( main
  ) where

import Prelude
import Effect (Effect)
import ShareTarget.Event (registerShareEvent)
import Web.HTML (window)

main :: Effect Unit
main = do
  registerShareEvent =<< window
