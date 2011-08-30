{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , ExistentialQuantification
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
import Diagrams.TwoD.Path (Clip(..))
import Diagrams.TwoD.Text
import Diagrams.TwoD.Image
import Diagrams.TwoD.Adjust (adjustDia2D, adjustSize)

import Graphics.UI.Gtk (DrawableClass)
import qualified Graphics.UI.Gtk.Cairo as CG

import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as CM

import Control.Applicative ((<$>))
import Control.Monad.State
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (isSuffixOf)

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
    forall dw. (DrawableClass dw) =>
    GTK { gtkWindow :: dw               -- ^ the window on which to draw
        , gtkSize   :: Maybe (Int, Int) -- ^ the size of the output is given in pixels. If Nothing, rescaling should not be performed.
        , gtkBypass :: Bool             -- ^ The adjustDia step should be bypassed during rendering
        }
    -- | PNG is unique, in that it is not a vector format
  | PNG { pngSize :: (Int, Int)       -- ^ the size of the output is given in pixels
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

type RenderM a = StateT () C.Render a  -- no state for now

-- simple, stupid implementations of save and restore for now, since
-- it suffices to just reset the text alignment to "centered" on
-- restore.  But if need be we can switch to a more sophisticated
-- implementation using an "undoable state" monad which lets you save
-- (push state onto a stack) and restore (pop from the stack).

save :: RenderM ()
save = lift C.save

restore :: RenderM ()
restore = lift C.restore

instance Backend Cairo R2 where
  data Render  Cairo R2 = C (RenderM ())
  type Result  Cairo R2 = (IO (), C.Render ())
  data Options Cairo R2 = CairoOptions
          { fileName     :: String       -- ^ the name of the file you want generated
          , outputFormat :: OutputFormat -- ^ the output format and associated options
          }

  withStyle _ s t (C r) = C $ do
    save
    cairoMiscStyle s
    r
    lift $ do
      cairoTransf t
      cairoStrokeStyle s
      C.stroke
    restore

  doRender _ options (C r) = (renderIO, r')
    where r' = evalStateT r ()
          renderIO = do
            let surfaceF s = C.renderWith s r'
                file = fileName options
            case outputFormat options of
              GTK win _ _ -> CG.renderWithDrawable win r'
              PNG (w,h) ->
                C.withImageSurface C.FormatARGB32 w h $ \surface -> do
                  surfaceF surface
                  C.surfaceWriteToPNG surface file
              PS  (w,h) -> C.withPSSurface  file w h surfaceF
              PDF (w,h) -> C.withPDFSurface file w h surfaceF
              SVG (w,h) -> C.withSVGSurface file w h surfaceF

  adjustDia c opts d = if bypass (outputFormat opts)
                         then d
                         else adjustDia2D (getSize . outputFormat) c opts (d # reflectY)
    where getSize (GTK _ (Just (pw,ph)) _) = (fromIntegral pw, fromIntegral ph)
          getSize (GTK _ Nothing _)        = size2D d
          getSize (PNG (pw,ph)) = (fromIntegral pw, fromIntegral ph)
          getSize (PS  sz) = sz
          getSize (PDF sz) = sz
          getSize (SVG sz) = sz
          bypass  (GTK _ _ x)   = x
          bypass  _             = False

renderC :: (Renderable a Cairo, V a ~ R2) => a -> RenderM ()
renderC a = case (render Cairo a) of C r -> r

cairoMiscStyle :: Style v -> RenderM ()
cairoMiscStyle s =
  sequence_
  . catMaybes $ [ handle clip
                , handle fSize
                , handleFontFace
                , handle fColor
                ]
  where handle :: AttributeClass a => (a -> RenderM ()) -> Maybe (RenderM ())
        handle f = f `fmap` getAttr s
        clip     = mapM_ (\p -> renderC p >> lift C.clip) . getClip
        fSize    = lift . C.setFontSize . getFontSize
        fFace    = fromMaybe "" $ getFont <$> getAttr s
        fSlant   = fromFontSlant  . fromMaybe FontSlantNormal
                 $ getFontSlant  <$> getAttr s
        fWeight  = fromFontWeight . fromMaybe FontWeightNormal
                 $ getFontWeight <$> getAttr s
        handleFontFace = Just . lift $ C.selectFontFace fFace fSlant fWeight
        fColor c = lift $ setSource (getFillColor c) s

fromFontSlant :: FontSlant -> C.FontSlant
fromFontSlant FontSlantNormal   = C.FontSlantNormal
fromFontSlant FontSlantItalic   = C.FontSlantItalic
fromFontSlant FontSlantOblique  = C.FontSlantOblique

fromFontWeight :: FontWeight -> C.FontWeight
fromFontWeight FontWeightNormal = C.FontWeightNormal
fromFontWeight FontWeightBold   = C.FontWeightBold

cairoStrokeStyle :: Style v -> C.Render ()
cairoStrokeStyle s =
  sequence_
  . catMaybes $ [ handle fColor
                , handle lColor  -- see Note [color order]
                , handle lWidth
                , handle lCap
                , handle lJoin
                , handle lDashing
                ]
  where handle :: (AttributeClass a) => (a -> C.Render ()) -> Maybe (C.Render ())
        handle f = f `fmap` getAttr s
        fColor c = setSource (getFillColor c) s >> C.fillPreserve
        lColor c = setSource (getLineColor c) s
        lWidth = C.setLineWidth . getLineWidth
        lCap   = C.setLineCap . fromLineCap . getLineCap
        lJoin  = C.setLineJoin . fromLineJoin . getLineJoin
        lDashing (getDashing -> Dashing ds offs) =
          C.setDash ds offs

setSource :: Color c => c -> Style v -> C.Render ()
setSource c s = C.setSourceRGBA r g b a'
  where (r,g,b,a) = colorToRGBA c
        a'        = case getOpacity <$> getAttr s of
                      Nothing -> a
                      Just d  -> a * d


cairoTransf :: T2 -> C.Render ()
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
  render _ ell = C . lift $ do
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
  render _ (Linear v) = C . lift $ uncurry C.relLineTo v
  render _ (Cubic (x1,y1) (x2,y2) (x3,y3)) = C . lift $ C.relCurveTo x1 y1 x2 y2 x3 y3

instance Renderable (Trail R2) Cairo where
  render _ (Trail segs c) = C $ do
    mapM_ renderC segs
    lift $ when c C.closePath

instance Renderable (Path R2) Cairo where
  render _ (Path trs) = C $ lift C.newPath >> F.mapM_ renderTrail trs
    where renderTrail (P p, tr) = do
            lift $ uncurry C.moveTo p
            renderC tr


-- Can only do PNG files at the moment...
instance Renderable Image Cairo where
  render _ (Image file sz tr) = C . lift $ do
    if ".png" `isSuffixOf` file
      then do
        C.save
        cairoTransf (tr <> reflectionY)
        pngSurf <- liftIO $ C.imageSurfaceCreateFromPNG file
        w <- C.imageSurfaceGetWidth pngSurf
        h <- C.imageSurfaceGetHeight pngSurf
        let s = (adjustSize sz (fromIntegral w, fromIntegral h))
        cairoTransf s
        C.setSourceSurface pngSurf (-fromIntegral w / 2)
                                   (-fromIntegral h / 2)
        C.paint
        C.restore
      else
        liftIO . putStr . unlines $
          [ "Warning: Cairo backend can currently only render embedded"
          , "  images in .png format.  Ignoring <" ++ file ++ ">."
          ]

-- see http://www.cairographics.org/tutorial/#L1understandingtext
instance Renderable Text Cairo where
  render _ (Text tr str) = C $ do
    lift $ do
      C.save
      -- XXX should use reflection font matrix here instead?
      cairoTransf (tr <> reflectionY)
      tExt <- C.textExtents str
      let w    = C.textExtentsWidth tExt
          h    = C.textExtentsHeight tExt
          refX = -w/2 - C.textExtentsXbearing tExt
          refY = -h/2 - C.textExtentsYbearing tExt
          P (newX, newY) = origin
      cairoTransf (moveOriginBy (-newX - refX, newY - refY) mempty)
      C.showText str
      C.restore

{-
-- | A @TextRect@ is a utility object which acts like a rectangle for
--   the purposes of its bounding function and transformations, but
--   also accumulates any transformations performed on it in a
--   separate field.  This is useful for learning what transformation
--   we should do to some text in order to align it as requested,
--   since for the purposes of alignment we think of the text as
--   occupying a rectangular region.
data TextRect = TextRect (Bounds R2) T2

mkTextRect :: Double -> Double -> TextRect
mkTextRect w h = TextRect (bounds (rect w h :: Diagram Cairo R2)) mempty

type instance V TextRect = R2

instance Boundable TextRect where
  getBounds (TextRect b _) = b

instance HasOrigin TextRect where
  moveOriginTo p (TextRect b t) = TextRect (moveOriginTo p b)
                                           (moveOriginTo p t)

instance Transformable TextRect where
  transform t1 (TextRect b t2) = TextRect (transform t1 b) (t1 <> t2)
-}
