-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Cairo
-- Copyright   :  (c) 2011 Diagrams-cairo team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A full-featured rendering backend for diagrams using the
-- cairo rendering engine.
--
-----------------------------------------------------------------------------
module Diagrams.Backend.Cairo

  ( -- * Cairo-supported output formats
    OutputType(..)

    -- * Cairo-specific options
    -- $CairoOptions
  , Options(..)

    -- * Backend token
  , Cairo(..)
  ) where

import Diagrams.Backend.Cairo.Internal