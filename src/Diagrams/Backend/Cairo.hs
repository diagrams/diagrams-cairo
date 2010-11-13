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
  PNG { pngSize :: (Int, Int) -- ^ the size of the output is given in pixels
      } |
  PS { psSize :: (Double, Double) -- ^ the size of the output is given in points
     } |
  PDF { pdfSize :: (Double, Double) -- ^ the size of the output is given in points
      } |
  SVG { svgSize :: (Double, Double) -- ^ the size of the output is given in points
      }

instance Monoid (C.Render ()) where
  mempty  = return ()
  mappend = (>>)

instance Backend Cairo where
  type BSpace Cairo = P2
  type Render Cairo = C.Render ()
  type Result Cairo = IO ()
  data Options Cairo = CairoOptions
          { fileName :: String -- ^ the name of the file you want generated
          , outputFormat :: OutputFormat -- ^ the output format and associated options
          }

  withStyle _ s r = do
    C.save
    cairoStyle s
    r
    C.restore

  doRender _ options r =
    let surfaceF surface = C.renderWith surface r
    in  case outputFormat options of
          PNG (w,h) -> do
            C.withImageSurface C.FormatARGB32 w h $ \surface -> do
              surfaceF surface
              C.surfaceWriteToPNG surface (fileName options)
          PS  (w,h) -> C.withPSSurface (fileName options) w h surfaceF
          PDF (w,h) -> C.withPDFSurface (fileName options) w h surfaceF
          SVG (w,h) -> C.withSVGSurface (fileName options) w h surfaceF

cairoStyle :: Style -> C.Render ()
cairoStyle s = mconcat . catMaybes . map (flip fmap (getAttr s)) $ [hLColor]
  where hLColor (SomeColor c) = do
          let (r,g,b,a) = colorToRGBA c
          C.setSourceRGBA r g b a

instance Renderable Box Cairo where
  render _ (Box v1 v2 v3 v4) = do
    C.newPath
    uncurry C.moveTo v1
    uncurry C.lineTo v2
    uncurry C.lineTo v3
    uncurry C.lineTo v4
    C.closePath
    C.stroke

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
    C.stroke

instance Renderable (Segment P2) Cairo where
  render _ (Linear v) = uncurry C.relLineTo v
  render _ (Cubic (x1,y1) (x2,y2) (x3,y3)) = C.relCurveTo x1 y1 x2 y2 x3 y3

instance Renderable (Path P2) Cairo where
  render _ (Path c v segs) = do
    C.newPath
    uncurry C.moveTo v
    mapM_ (render Cairo) segs
    when c $ C.closePath
    C.stroke
