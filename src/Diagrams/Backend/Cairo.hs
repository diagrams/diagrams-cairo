{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}
module Diagrams.Backend.Cairo 
       
  ( Cairo(..) -- rendering token 

  , Option(..) -- for CairoOptions, rendering options specific to Cairo
  , OutputFormat(..) -- output format options
  ) where

import qualified Graphics.Rendering.Cairo as C

import Graphics.Rendering.Diagrams

import Diagrams.TwoD
import Diagrams.Path

data Cairo = Cairo

data OutputFormat = 
  PNG { pngSize :: (Int, Int) -- in pixels
      } |
  PS { psSize :: (Double, Double) -- in points
     } |
  PDF { pdfSize :: (Double, Double) -- in points
      } |
  SVG { svgSize :: (Double, Double) -- in points 
      }

instance Backend Cairo where
  type BSpace Cairo = P2
  type Render Cairo = C.Render ()
  type Result Cairo = IO ()
  data Option Cairo = CairoOptions  
          { fileName :: String
          , outputFormat :: OutputFormat
          }

  renderDia _ options d = let
    	surfaceF surface = C.renderWith surface (mapM_ (render Cairo) (prims d))
   in case outputFormat options of
          PNG (w,h) -> do
            C.withImageSurface C.FormatARGB32 w h $ \surface -> do
              surfaceF surface
              C.surfaceWriteToPNG surface (fileName options)
          PS  (w,h) -> C.withPSSurface (fileName options) w h surfaceF
          PDF (w,h) -> C.withPDFSurface (fileName options) w h surfaceF
          SVG (w,h) -> C.withSVGSurface (fileName options) w h surfaceF
          
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

instance Renderable (RelPath P2) Cairo where
  render _ (RelPath segs) = do
    mapM_ (render Cairo) segs

instance Renderable (Path P2) Cairo where
  render _ (Path v r) = do
    C.newPath
    uncurry C.moveTo v
    render Cairo r
    C.stroke
