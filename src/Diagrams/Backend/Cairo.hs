{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , TypeSynonymInstances
           , DeriveDataTypeable
           , ViewPatterns
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Cairo.CmdLine
-- Copyright   :  (c) 2011 Diagrams-cairo team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A full-featured rendering backend for diagrams using Cairo.
--
-----------------------------------------------------------------------------
module Diagrams.Backend.Cairo

  ( Cairo(..)        -- rendering token

  , Options(..)      -- for CairoOptions, rendering options specific to Cairo
  , OutputFormat(..) -- output format options
  ) where

import Graphics.Rendering.Diagrams.Transform

import Diagrams.Prelude
import Diagrams.TwoD.Ellipse

import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as CM

import Control.Monad (when)
import Data.Maybe (catMaybes)

import Data.Monoid
import qualified Data.Foldable as F

import Data.Typeable

-- | This data declaration is simply used as a token to distinguish
--   this rendering engine.
data Cairo = Cairo
  deriving (Eq,Ord,Read,Show,Typeable)

-- | Cairo is able to output to several file formats, which each have
--   their own associated properties that affect the output.
data OutputFormat =
    -- | PNG is unique, in that it is not a vector format
    PNG { pngSize :: (Int, Int)       -- ^ the size of the output is given in pixels
        }
  | PS  { psSize  :: (Double, Double) -- ^ the size of the output is given in points
        }
  | PDF { pdfSize :: (Double, Double) -- ^ the size of the output is given in points
        }
  | SVG { svgSize :: (Double, Double) -- ^ the size of the output is given in points
        }

instance Monoid (Render Cairo R2) where
  mempty  = C $ return ()
  (C r1) `mappend` (C r2) = C (r1 >> r2)

instance Backend Cairo R2 where
  data Render  Cairo R2 = C (C.Render ())
  type Result  Cairo R2 = (IO (), C.Render ())
  data Options Cairo R2 = CairoOptions
          { fileName     :: String       -- ^ the name of the file you want generated
          , outputFormat :: OutputFormat -- ^ the output format and associated options
          }

  withStyle _ s t (C r) = C $ do
    C.save
    r
    cairoTransf t
    cairoStyle s
    C.stroke
    C.restore

  doRender _ options (C r) = (renderIO, r)
    where renderIO = do
            let surfaceF s = C.renderWith s r
                file = fileName options
            case outputFormat options of
              PNG (w,h) ->
                C.withImageSurface C.FormatARGB32 w h $ \surface -> do
                  surfaceF surface
                  C.surfaceWriteToPNG surface file
              PS  (w,h) -> C.withPSSurface  file w h surfaceF
              PDF (w,h) -> C.withPDFSurface file w h surfaceF
              SVG (w,h) -> C.withSVGSurface file w h surfaceF

  -- Set the line width to 0.01 and line color to black (in case they
  -- were not set), freeze the diagram in its final form, and then do
  -- final adjustments to make it fit the requested size.
  adjustDia _ opts d = d' # lw 0.01 # lc black # freeze
                          # scale s
                          # translate tr
    where d'      = reflectY d   -- adjust for cairo's upside-down coordinate system
          (w,h)   = getSize $ outputFormat opts
          (wd,hd) = size2D d'
          xscale  = w / wd
          yscale  = h / hd
          s       = let s' = min xscale yscale
                    in  if isInfinite s' then 1 else s'
          tr      = (0.5 *. P (w,h)) .-. (s *. center2D d')

          getSize (PNG (pw,ph)) = (fromIntegral pw, fromIntegral ph)
          getSize (PS  sz) = sz
          getSize (PDF sz) = sz
          getSize (SVG sz) = sz

renderC :: (Renderable a Cairo, V a ~ R2) => a -> C.Render ()
renderC a = case (render Cairo a) of C r -> r

cairoStyle :: Style -> C.Render ()
cairoStyle s = sequence_
             . catMaybes $ [ handle fColor
                           , handle lColor  -- see Note [color order]
                           , handle lWidth
                           , handle lCap
                           , handle lJoin
                           , handle lDashing
                           ]
  where handle :: (AttributeClass a) => (a -> C.Render ()) -> Maybe (C.Render ())
        handle f = f `fmap` getAttr s
        fColor c = do
          let (r,g,b,a) = colorToRGBA . getFillColor $ c
          C.setSourceRGBA r g b a
          C.fillPreserve
        lColor c = do
          let (r,g,b,a) = colorToRGBA . getLineColor $ c
          C.setSourceRGBA r g b a
        lWidth = C.setLineWidth . getLineWidth
        lCap   = C.setLineCap . fromLineCap . getLineCap
        lJoin  = C.setLineJoin . fromLineJoin . getLineJoin
        lDashing (getDashing -> Dashing ds offs) =
          C.setDash ds offs

cairoTransf :: Transformation R2 -> C.Render ()
cairoTransf t = C.transform m
  where m = CM.Matrix a1 a2 b1 b2 c1 c2
        (a1,a2) = apply t (1,0)
        (b1,b2) = apply t (0,1)
        (c1,c2) = transl t

{- ~~~~ Note [color order]

   It's important for the line and fill colors to be handled in the
   given order (fill color first, then line color) because of the way
   Cairo handles them (both are taken from the sourceRGBA).
-}

fromLineCap :: LineCap -> C.LineCap
fromLineCap LineCapButt   = C.LineCapButt
fromLineCap LineCapRound  = C.LineCapRound
fromLineCap LineCapSquare = C.LineCapSquare

fromLineJoin :: LineJoin -> C.LineJoin
fromLineJoin LineJoinMiter = C.LineJoinMiter
fromLineJoin LineJoinRound = C.LineJoinRound
fromLineJoin LineJoinBevel = C.LineJoinBevel

instance Renderable Ellipse Cairo where
  render _ ell = C $ do
    let P (xc,yc) = ellipseCenter ell
        (xs,ys)   = ellipseScale ell
        Rad th    = ellipseAngle ell
    C.newPath
    C.save
    C.translate xc yc
    C.rotate th
    C.scale xs ys
    C.arc 0 0 1 0 (2*pi)
    C.closePath
    C.restore

instance Renderable (Segment R2) Cairo where
  render _ (Linear v) = C $ uncurry C.relLineTo v
  render _ (Cubic (x1,y1) (x2,y2) (x3,y3)) = C $ C.relCurveTo x1 y1 x2 y2 x3 y3

instance Renderable (Trail R2) Cairo where
  render _ (Trail segs c) = C $ do
    mapM_ renderC segs
    when c C.closePath

instance Renderable (Path R2) Cairo where
  render _ (Path trs) = C $ C.newPath >> F.mapM_ renderTrail trs
    where renderTrail (P p, tr) = do
            uncurry C.moveTo p
            renderC tr