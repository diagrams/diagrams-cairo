{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Cairo.Text
-- Copyright   :  (c) 2011 Diagrams-cairo team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This module provides convenience functions for querying information
-- from cairo.  In particular, this provides utilities for information
-- about fonts, and creating text primitives with bounds based on the
-- font being used. To render text with automatically determined
-- envelopes, use 'textLineBounded', 'textLineBoundedIO',
-- 'textVisualBounded', or 'textVisualBoundedIO'.
--
-- Many of these functions take a 'Style' 'R2' parameter, determining the
-- style to apply to the text before rendering / querying information about
-- the text.  These 'Style' 'R2' parameters can be created a variety of ways,
-- but the most direct will likely be by applying style-transforming functions
-- such as 'font', 'fontSize', 'fontSlant', and 'fontWeight' to 'mempty'.
-- This works because there are instances of 'HasStyle' and 'Monoid' for
-- @'Style' v@.
--
-----------------------------------------------------------------------------
module Diagrams.Backend.Cairo.Text
  (
    -- * Primitives

    -- | These create diagrams instantiated with extents-based envelopes
    textLineBoundedIO, textVisualBoundedIO

    -- ** Unsafe

    -- | These are convenient unsafe variants of the above operations
    --   postfixed with \"IO\". They should be pretty well-behaved as the
    --   results just depend on the parameters and the font information
    --   (which ought to stay the same during a given execution).

  , kerningCorrection, textLineBounded, textVisualBounded

    -- * Extents

    -- ** Data Structures
  , TextExtents(..), FontExtents(..)

    -- ** Queries

  , getTextExtents, getFontExtents, getExtents
  , kerningCorrectionIO

    -- * Utilities
  , queryCairo, unsafeCairo
  , cairoWithStyle
  ) where

import Diagrams.Backend.Cairo.Internal
import Diagrams.Prelude

import Control.Monad.State
import System.IO.Unsafe

import qualified Graphics.Rendering.Cairo as C

-- | Executes a cairo action on a dummy, zero-size image surface, in order to
--   query things like font information.
queryCairo :: C.Render a -> IO a
queryCairo c = C.withImageSurface C.FormatA1 0 0 (`C.renderWith` c)

-- | Unsafely invokes @queryCairo@.
unsafeCairo :: C.Render a -> a
unsafeCairo = unsafePerformIO . queryCairo

-- | Executes the given cairo action, with styling applied.
--   This does not do all styling - just attributes that are processed by
--   \"cairoMiscStyle\", which does clip, fill color, fill rule, and,
--   importantly for this module, font face, style, and weight.
cairoWithStyle :: C.Render a -> Style R2 -> C.Render a
cairoWithStyle f style = do
  C.save
  evalStateT (cairoMiscStyle style) ()
  result <- f
  C.restore
  return result

-- | A more convenient data structure for the results of a text-extents query.
data TextExtents = TextExtents
  { bearing, textSize, advance :: R2 }

processTextExtents :: C.TextExtents -> TextExtents
processTextExtents (C.TextExtents  xb yb  w h  xa ya)
                    = TextExtents (r2 (xb,yb)) (r2 (w,h)) (r2 (xa,ya))

-- | Get the extents of a string of text, given a style to render it with.
getTextExtents :: Style R2 -> String -> C.Render TextExtents
getTextExtents style txt
  = cairoWithStyle (processTextExtents <$> C.textExtents txt) style

-- | A more convenient data structure for the results of a font-extents query.
data FontExtents = FontExtents
  { ascent, descent, height :: Double
  , maxAdvance :: R2
  }

processFontExtents :: C.FontExtents -> FontExtents
processFontExtents (C.FontExtents a d h  mx my)
                    = FontExtents a d h (r2 (mx,my))

-- | Gets the intrinsic extents of a font.
getFontExtents :: Style R2 -> C.Render FontExtents
getFontExtents style
  = cairoWithStyle (processFontExtents <$> C.fontExtents) style

-- | Gets both the "FontExtents" and "TextExtents" of the string with the a
--   particular style applied.  This is more efficient than calling both
--   @getFontExtents@ and @getTextExtents@.
getExtents :: Style R2 -> String -> C.Render (FontExtents, TextExtents)
getExtents style str = cairoWithStyle (do
    fe <- processFontExtents <$> C.fontExtents
    te <- processTextExtents <$> C.textExtents str
    return (fe, te)
  ) style

-- | Queries the amount of horizontal offset that needs to be applied in order to
--   position the second character properly, in the event that it is @hcat@-ed
--   @baselineText@.
kerningCorrectionIO :: Style R2 -> Char -> Char -> IO Double
kerningCorrectionIO style a b = do
  let ax t = fst . unr2 . advance <$> queryCairo (getTextExtents style t)
  l  <- ax [a, b]
  la <- ax [a]
  lb <- ax [b]
  return $ l - la - lb

-- | Creates text diagrams with their envelopes set such that using
--   @vcat . map (textLineBounded style)@ stacks them in the way that
--   the font designer intended.
textLineBoundedIO :: Style R2 -> String -> IO (Diagram Cairo R2)
textLineBoundedIO style str = do
  (fe, te) <- queryCairo $ getExtents style str
  let box = fromCorners (p2 (0,      negate $ descent fe))
                        (p2 (fst . unr2 $ advance te, ascent fe))
  return . setEnvelope (getEnvelope box) . applyStyle style $ baselineText str

-- | Creates a text diagram with its envelope set to enclose the glyphs of the text,
--   including leading (though not trailing) whitespace.
textVisualBoundedIO :: Style R2 -> String -> IO (Diagram Cairo R2)
textVisualBoundedIO style str = do
  te <- queryCairo $ getTextExtents style str
  let box = fromCorners (origin .+^ bearing te)
                        ((origin .+^ bearing te) .+^ textSize te)
  return . setEnvelope (getEnvelope box) . applyStyle style $ baselineText str

kerningCorrection :: Style R2 -> Char -> Char -> Double
kerningCorrection style a = unsafePerformIO . kerningCorrectionIO style a

textLineBounded, textVisualBounded :: Style R2 -> String -> Diagram Cairo R2
textLineBounded   style = unsafePerformIO . textLineBoundedIO   style
textVisualBounded style = unsafePerformIO . textVisualBoundedIO style