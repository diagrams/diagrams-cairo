-- |
-- Module      :  Diagrams.Backend.Cairo.Text
-- Copyright   :  (c) 2015 Diagrams-cairo team (see LICENSE)
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
-- Many of these functions take a 'Style' 'V2' 'Double' parameter,
-- determining the style to apply to the text before rendering /
-- querying information about the text.  These 'Style' 'V2' 'Double'
-- parameters can be created a variety of ways, but the most direct
-- will likely be by applying style-transforming functions such as
-- 'font', 'fontSize', 'fontSlant', and 'fontWeight' to 'mempty'.
-- This works because there are instances of 'HasStyle' and 'Monoid'
-- for @'Style' v@.

module Diagrams.Backend.Cairo.Text
       (
         -- | These create diagrams instantiated with extent-based envelopes.
         textLineBoundedIO
       , textVisualBoundedIO

         -- * Utilities
       , queryCairo, unsafeCairo
       ) where

import           Diagrams.Backend.Cairo.Internal
import qualified Diagrams.BoundingBox            as BB
import           Diagrams.Prelude                hiding (height, view)
import           Diagrams.TwoD.Text              hiding (font)

import qualified Graphics.Rendering.Cairo        as C
import qualified Graphics.Rendering.Pango        as P

import           System.IO.Unsafe

-- | Executes a cairo action on a dummy, zero-size image surface, in order to
--   query things like font information.
queryCairo :: C.Render a -> IO a
queryCairo c = C.withImageSurface C.FormatA1 0 0 (`C.renderWith` c)

-- | Unsafely invokes 'queryCairo' using 'unsafePerformIO'.
unsafeCairo :: C.Render a -> a
unsafeCairo = unsafePerformIO . queryCairo

-- | Creates text diagrams with their envelopes set such that using
--   @'vcat' . map ('textLineBounded' style)@ stacks them in the way that
--   the font designer intended.  Pango refers to this as logical extents.
textLineBoundedIO :: Style V2 Double -> Text Double -> IO (Diagram Cairo)
textLineBoundedIO = textLineIO fst

-- | Creates a text diagram with its envelope set to enclose the glyphs of the text,
--   including leading (though not trailing) whitespace.
textVisualBoundedIO :: Style V2 Double -> Text Double -> IO (Diagram Cairo)
textVisualBoundedIO = textLineIO snd

-- | Abstract common code from @textLineBoundedIO@ and @textVisualBoundedIO@
-- textLineIO :: ((a,a) -> a) -> Style V2 Double -> Text Double -> IO (Diagram Cairo)
textLineIO :: ((P.PangoRectangle,P.PangoRectangle) -> P.PangoRectangle) -> Style V2 Double -> Text Double -> IO (Diagram Cairo)
textLineIO pick sty txt = do
    layout <- queryCairo $ layoutStyledText sty txt
    P.PangoRectangle x y  w h <- pick <$> P.layoutGetExtents layout
    let bb = BB.fromCorners (mkP2 x y) (mkP2 (x + w) (y + h))
    return $ mkQD (Prim txt) (getEnvelope bb) mempty mempty mempty
