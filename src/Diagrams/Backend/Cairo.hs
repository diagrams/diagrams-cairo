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

import Graphics.Rendering.Diagrams

import Diagrams.Attributes
import Diagrams.TwoD
import Diagrams.TwoD.Ellipse
import Diagrams.TwoD.Shapes
import Diagrams.Segment
import Diagrams.Path

import Control.Monad (when)
import Data.Maybe (catMaybes)

import Data.Monoid

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
    in  case outputFormat options of
          PNG (w,h) -> do
            C.withImageSurface C.FormatARGB32 w h $ \surface -> do
              surfaceF surface
              C.surfaceWriteToPNG surface (fileName options)
          PS  (w,h) -> C.withPSSurface  (fileName options) w h surfaceF
          PDF (w,h) -> C.withPDFSurface (fileName options) w h surfaceF
          SVG (w,h) -> C.withSVGSurface (fileName options) w h surfaceF

cairoStyle :: Style -> C.Render ()
cairoStyle s = mconcat . catMaybes $ [ handle fColor
                                     , handle lColor  -- see Note [color order]
                                     , handle lWidth
                                     , handle lCap
                                     , handle lJoin
                                     , handle lDashing
                                     ]
  where handle f = fmap f (getAttr s)
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

instance Renderable Box Cairo where
  render _ (Box (P v1) (P v2) (P v3) (P v4)) = do
    C.newPath
    uncurry C.moveTo v1
    uncurry C.lineTo v2
    uncurry C.lineTo v3
    uncurry C.lineTo v4
    C.closePath

instance Renderable Ellipse Cairo where
  render _ ell@(Ellipse a b c d e f) = do
    let (xc,yc,xs,ys,th) = ellipseCenterScaleAngle ell
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

instance Renderable (Path R2) Cairo where
  render _ (Path c (P v) segs) = do
    C.newPath
    uncurry C.moveTo v
    mapM_ (render Cairo) segs
    when c $ C.closePath
