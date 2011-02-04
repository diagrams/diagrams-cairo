{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}
{-|
  The Cairo backend.
-}
module Diagrams.Backend.Cairo

  ( Cairo(..) -- rendering token

  , Options(..) -- for CairoOptions, rendering options specific to Cairo
  , OutputFormat(..) -- output format options
  ) where

import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as CM

import Diagrams.Prelude

import Graphics.Rendering.Diagrams.Transform

import Diagrams.TwoD.Ellipse
import Diagrams.TwoD.Shapes

import Control.Monad (when)
import Data.Maybe (catMaybes)

import Data.Monoid
import qualified Data.Foldable as F

-- | This data declaration is simply used as a token to distinguish
--   this rendering engine.
data Cairo = Cairo

-- | Cairo is able to output to several file formats, which each have
--   their own associated properties that affect the output.
data OutputFormat =
    -- | PNG is unique, in that it is not a vector format
    PNG { pngSize :: (Int, Int)       -- ^ the size of the output is given in pixels
        }
  | PS  { psSize :: (Double, Double)  -- ^ the size of the output is given in points
        }
  | PDF { pdfSize :: (Double, Double) -- ^ the size of the output is given in points
        }
  | SVG { svgSize :: (Double, Double) -- ^ the size of the output is given in points
        }

instance Monoid (C.Render ()) where
  mempty  = return ()
  mappend = (>>)

instance Backend Cairo where
  type BSpace Cairo = R2
  type Render Cairo = C.Render ()
  type Result Cairo = IO ()
  data Options Cairo = CairoOptions
          { fileName     :: String       -- ^ the name of the file you want generated
          , outputFormat :: OutputFormat -- ^ the output format and associated options
          }

  withStyle _ s r = do
    C.save
    r
    cairoStyle s
    C.stroke
    C.restore

  doRender _ options r =
    let surfaceF surface = C.renderWith surface r
        file = fileName options
    in  case outputFormat options of
          PNG (w,h) -> do
            C.withImageSurface C.FormatARGB32 w h $ \surface -> do
              surfaceF surface
              C.surfaceWriteToPNG surface file
          PS  (w,h) -> C.withPSSurface  file w h surfaceF
          PDF (w,h) -> C.withPDFSurface file w h surfaceF
          SVG (w,h) -> C.withSVGSurface file w h surfaceF

  adjustDia _ opts d = translate tr . scale s $ d'
    where d'      = reflectY d   -- adjust for cairo's upside-down coordinate system
          (w,h)   = getSize $ outputFormat opts
          (wd,hd) = size2D d'
          xscale  = if wd == 0 then 1 else w / wd
          yscale  = if hd == 0 then 1 else h / hd
          s       = min xscale yscale
          tr      = (0.5 *. P (w,h)) .-. (s *. center2D d')

          getSize (PNG (w,h)) = (fromIntegral w, fromIntegral h)
          getSize (PS  s) = s
          getSize (PDF s) = s
          getSize (SVG s) = s

cairoStyle :: Style R2 -> C.Render ()
cairoStyle s = mconcat . catMaybes $ [ handle fColor
                                     , handle lColor  -- see Note [color order]
                                     , handle lWidth
                                     , handle lCap
                                     , handle lJoin
                                     , handle lDashing
                                     ]
  where handle :: (AttributeClass t) => (t -> C.Render ()) -> Maybe (C.Render ())
        handle f = renderAttr f `fmap` getAttr s
        renderAttr :: (AttributeClass a) => (a -> C.Render ()) -> (a, Transformation R2) -> C.Render ()
        renderAttr f (a,t) = do
          cairoTransf t
          f a
        fColor (FillColor (SomeColor c)) = do
          let (r,g,b,a) = colorToRGBA c
          C.setSourceRGBA r g b a
          C.fillPreserve
        lColor (LineColor (SomeColor c)) = do
          let (r,g,b,a) = colorToRGBA c
          C.setSourceRGBA r g b a
        lWidth (LineWidth w) = do
          C.setLineWidth w
        lCap lc = do
          C.setLineCap (fromLineCap lc)
        lJoin lj = do
          C.setLineJoin (fromLineJoin lj)
        lDashing (Dashing ds offs) = do
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
  render _ ell = do
    let P (xc,yc) = ellipseCenter ell
        (xs,ys)   = ellipseScale ell
        th        = ellipseAngle ell
    C.newPath
    C.save
    C.translate xc yc
    C.rotate th
    C.scale xs ys
    C.arc 0 0 1 0 (2*pi)
    C.closePath
    C.restore

instance Renderable (Segment R2) Cairo where
  render _ (Linear v) = uncurry C.relLineTo v
  render _ (Cubic (x1,y1) (x2,y2) (x3,y3)) = C.relCurveTo x1 y1 x2 y2 x3 y3

instance Renderable (Trail R2) Cairo where
  render _ (Trail segs c) = do
    mapM_ (render Cairo) segs
    when c $ C.closePath

instance Renderable (Path R2) Cairo where
  render _ (Path trs) = C.newPath >> F.mapM_ renderTrail trs
    where renderTrail (tr, P p) = do
            uncurry C.moveTo p
            render Cairo tr