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
-- GHC 7.4.0 regression?
#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL__ < 704
  , Options(..)
#endif

    -- * Backend token
  , Cairo(..)
  ) where

import Diagrams.Backend.Cairo.Internal

-- $CairoOptions
--
-- Unfortunately, Haddock does not yet support documentation for
-- associated data families, so we must just provide it manually.
-- This module defines
--
-- > data family Options Cairo R2 = CairoOptions
-- >           { cairoFileName   :: String     -- ^ The name of the file you want generated
-- >           , cairoSizeSpec   :: SizeSpec2D -- ^ The requested size of the output
-- >           , cairoOutputType :: OutputType -- ^ the output format and associated options
-- >           }
--
-- So, for example, you could call the 'renderDia' function (from
-- "Graphics.Rendering.Diagrams.Core") like this:
--
-- > renderDia Cairo (CairoOptions "foo.png" (Width 250) PNG) myDiagram
--
-- /Important note/: a bug in GHC 7.0.x prevents re-exporting this
-- data family.  To bring CairoOptions into scope when using GHC 7.0.x
-- you must import "Diagrams.Backend.Cairo.Internal".
