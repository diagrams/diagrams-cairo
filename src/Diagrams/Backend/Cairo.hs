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
-- To invoke the cairo backend, you have three options.
--
-- * You can use the "Diagrams.Backend.Cairo.CmdLine" module to create
--   standalone executables which output images when invoked.
--
-- * You can use the 'renderCairo' function provided by this module,
--   which gives you more flexible programmatic control over when and
--   how images are output (making it easy to, for example, write a
--   single program that outputs multiple images, or one that outputs
--   images dynamically based on user input, and so on).
--
-- * Finally, for the most flexibility, you can directly
--   use methods from the
--   'Diagrams.Core.Types.Backend' instance for @Cairo@.  In particular,
--   'Diagrams.Core.Types.renderDia' has the generic type
--
-- > renderDia :: b -> Options b v -> QDiagram b v m -> Result b v
--
-- (omitting a few type class constraints).  @b@ represents the
-- backend type, @v@ the vector space, @n@ the numeric field, and @m@
-- the type of monoidal query annotations on the diagram.  'Options'
-- and 'Result' are associated data and type families, respectively,
-- which yield the type of option records and rendering results
-- specific to any particular backend.  For @b ~ Cairo@ @n ~ Double@,
-- and @v ~ V2@, we have
--
-- > data family Options Cairo V2 Double = CairoOptions
-- >          { _cairoFileName     :: String     -- ^ The name of the file you want generated
-- >          , _cairoSizeSpec     :: SizeSpec2D -- ^ The requested size of the output
-- >          , _cairoOutputType   :: OutputType -- ^ the output format and associated options
-- >          , _cairoBypassAdjust :: Bool       -- ^ Should the 'adjustDia' step be bypassed during rendering?
-- >          }
--
-- @
-- type family Result Cairo V2 Double = (IO (), 'Graphics.Rendering.Cairo.Render' ())
-- @
--
-- So the type of 'renderDia' resolves to
--
-- @
-- renderDia :: Cairo -> Options Cairo V2 Double -> QDiagram Cairo V2 Double m -> (IO (), 'Graphics.Rendering.Cairo.Render' ())
-- @
--
-- which you could call like so:
--
-- @
-- renderDia Cairo (CairoOptions \"foo.png\" (Width 250) PNG False) (myDiagram :: Diagram Cairo V2 Double)
-- @
--
-- This would return a pair; the first element is an @IO ()@ action
-- which will write out @foo.png@ to disk, and the second is a cairo
-- rendering action which can be used, for example, to directly draw
-- to a Gtk window.  Note the type annotation on @myDiagram@ which may
-- be necessary to fix the type variable @m@; this example uses the
-- type synonym @Diagram b v = QDiagram b v Any@ to fix @m = Any@.
--
-----------------------------------------------------------------------------
module Diagrams.Backend.Cairo

  ( -- * Rendering
    renderCairo

    -- * Cairo-supported output formats
  , OutputType(..)

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
  , B
  ) where

import System.FilePath (takeExtension)

import Diagrams.Backend.Cairo.Internal
import Diagrams.Prelude

-- $CairoOptions
--
-- Unfortunately, Haddock does not yet support documentation for
-- associated data families, so we must just provide it manually.
-- This module defines
--
-- > data family Options Cairo V2 Double = CairoOptions
-- >           { _cairoFileName   :: String     -- ^ The name of the file you want generated
-- >           , _cairoSizeSpec   :: SizeSpec2D -- ^ The requested size of the output
-- >           , _cairoOutputType :: OutputType -- ^ the output format and associated options
-- >           }
--
-- See the documentation at the top of "Diagrams.Backend.Cairo" for
-- information on how to make use of this.
--
-- /Important note/: a bug in GHC 7.0.x and 7.4.1 prevents
-- re-exporting this data family.  (Strangely, this bug seems to be
-- present in 7.0 and 7.4 but not 7.2.) To bring CairoOptions into
-- scope when using GHC 7.0.x or 7.4 you must import
-- "Diagrams.Backend.Cairo.Internal".

-- | Render a diagram using the cairo backend, writing to the given
--   output file and using the requested size.  The output type (PNG,
--   PS, PDF, or SVG) is determined automatically from the output file
--   extension.
--
--   This function is provided as a convenience; if you need more
--   flexibility than it provides, you can call 'renderDia' directly,
--   as described above.
renderCairo :: FilePath -> SizeSpec2D Double -> Diagram Cairo V2 Double -> IO ()
renderCairo outFile sizeSpec d
  = fst (renderDia Cairo (CairoOptions outFile sizeSpec outTy False) d)
  where
    outTy =
      case takeExtension outFile of
        ".png" -> PNG
        ".ps"  -> PS
        ".pdf" -> PDF
        ".svg" -> SVG
        _      -> PNG
