{-# LANGUAGE TypeFamilies, CPP #-}

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

-- The below CPP hack is needed because GHC 7.0.x has a bug regarding
-- (re?)export of data family constructors; in particular the below
-- export causes the error "Not in scope: type constructor or class
-- `Options'" even though
-- http://www.haskell.org/haskellwiki/GHC/Type_families#Import_and_export
-- seems to indicate it should be supported.  When using 7.0.x one
-- must import Diagrams.Backend.Cairo.Internal in order to bring
-- CairoOptions into scope.
#if __GLASGOW_HASKELL__ >= 702
  , Options(..)
#endif

    -- * Backend token
  , Cairo(..)
  ) where

import Diagrams.Backend.Cairo.Internal