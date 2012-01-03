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
-- Module      :  Diagrams.Backend.Cairo.Internal
-- Copyright   :  (c) 2011 Diagrams-cairo team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Implementation of the diagrams Cairo backend.  This module exports the
-- internal helper functions, for usage elsewhere.
--
-----------------------------------------------------------------------------
module Diagrams.Backend.Cairo.Internal where

import Graphics.Rendering.Diagrams.Transform

import Diagrams.Prelude
import Diagrams.TwoD.Path (Clip(..), getFillRule)
import Diagrams.TwoD.Text
import Diagrams.TwoD.Image
import Diagrams.TwoD.Adjust (adjustDia2D, adjustSize)

import Graphics.UI.Gtk (DrawableClass)
import qualified Graphics.UI.Gtk.Cairo as CG

import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.Rendering.Cairo.Matrix as CM

import Control.Monad.State
import Data.Maybe (catMaybes, fromMaybe)
import Data.List (isSuffixOf)
import System.IO.Error

import qualified Data.Foldable as F

import Data.Typeable

-- | This data declaration is simply used as a token to distinguish
--   this rendering engine.
data Cairo = Cairo
  deriving (Eq,Ord,Read,Show,Typeable)

-- | Output types supported by cairo, including four different file
--   types (PNG, PS, PDF, SVG) as well as Gtk windows.
data OutputType =
    -- | Output directly to a Gtk window.  See "Diagrams.Backends.Cairo.Gtk".
    forall dw. (DrawableClass dw) =>
    GTK { gtkWindow :: dw
          -- ^ The window on which to draw.

        , gtkBypass :: Bool
          -- ^ Should the 'adjustDia' step be bypassed during rendering?
        }

  | PNG      -- ^ Portable Network Graphics output.
  | PS       -- ^ PostScript output
  | PDF      -- ^ Portable Document Format output.
  | SVG      -- ^ Scalable Vector Graphics output.

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

instance Backend Cairo R2 where
  data Render  Cairo R2 = C (RenderM ())
  type Result  Cairo R2 = (IO (), C.Render ())
  data Options Cairo R2 = CairoOptions
          { cairoFileName   :: String     -- ^ The name of the file you want generated
          , cairoSizeSpec   :: SizeSpec2D -- ^ The requested size of the output
          , cairoOutputType :: OutputType -- ^ the output format and associated options
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

  doRender _ (CairoOptions file size out) (C r) = (renderIO, r')
    where r' = evalStateT r ()
          renderIO = do
            let surfaceF s = C.renderWith s r'

                -- Everything except Dims is arbitrary. The backend
                -- should have first run 'adjustDia' to update the
                -- final size of the diagram with explicit dimensions,
                -- so normally we would only expect to get Dims anyway.
                (w,h) = case size of
                          Width w'   -> (w',w')
                          Height h'  -> (h',h')
                          Dims w' h' -> (w',h')
                          Absolute   -> (100,100)

            case out of
              GTK win _ -> CG.renderWithDrawable win r'
              PNG ->
                C.withImageSurface C.FormatARGB32 (round w) (round h) $ \surface -> do
                  surfaceF surface
                  C.surfaceWriteToPNG surface file
              PS  -> C.withPSSurface  file w h surfaceF
              PDF -> C.withPDFSurface file w h surfaceF
              SVG -> C.withSVGSurface file w h surfaceF

  adjustDia c opts d = if bypass (cairoOutputType opts)
                         then (opts,d)
                         else adjustDia2D cairoSizeSpec
                                          setCairoSizeSpec
                                          c opts (d # reflectY)
    where setCairoSizeSpec sz o = o { cairoSizeSpec = sz }
          bypass  (GTK _ x) = x
          bypass  _         = False

renderC :: (Renderable a Cairo, V a ~ R2) => a -> RenderM ()
renderC a = case (render Cairo a) of C r -> r

cairoMiscStyle :: Style v -> RenderM ()
cairoMiscStyle s =
  sequence_
  . catMaybes $ [ handle clip
                , handle fSize
                , handleFontFace
                , handle fColor
                , handle lFillRule
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
        lFillRule = lift . C.setFillRule . fromFillRule . getFillRule

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
        lWidth   = C.setLineWidth . getLineWidth
        lCap     = C.setLineCap . fromLineCap . getLineCap
        lJoin    = C.setLineJoin . fromLineJoin . getLineJoin
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

fromFillRule :: FillRule -> C.FillRule
fromFillRule Winding = C.FillRuleWinding
fromFillRule EvenOdd = C.FillRuleEvenOdd

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
        pngSurfChk <- liftIO (try $ C.imageSurfaceCreateFromPNG file)
        case pngSurfChk of
          Right pngSurf -> do
            w <- C.imageSurfaceGetWidth pngSurf
            h <- C.imageSurfaceGetHeight pngSurf
            cairoTransf $ adjustSize sz (fromIntegral w, fromIntegral h)
            C.setSourceSurface pngSurf (-fromIntegral w / 2)
                                       (-fromIntegral h / 2)
          Left _ ->
            liftIO . putStrLn $
              "Warning: can't read image file <" ++ file ++ ">"
        C.paint
        C.restore
      else
        liftIO . putStr . unlines $
          [ "Warning: Cairo backend can currently only render embedded"
          , "  images in .png format.  Ignoring <" ++ file ++ ">."
          ]

-- see http://www.cairographics.org/tutorial/#L1understandingtext
instance Renderable Text Cairo where
  render _ (Text tr al str) = C $ do
    lift $ do
      C.save
      -- XXX should use reflection font matrix here instead?
      cairoTransf (tr <> reflectionY)
      (refX, refY) <- case al of
        BoxAlignedText (xt, yt) -> do
          tExt <- C.textExtents str
          fExt <- C.fontExtents
          let l = C.textExtentsXbearing tExt
              r = C.textExtentsXadvance tExt
              b = C.fontExtentsDescent  fExt
              t = C.fontExtentsAscent   fExt
          return (lerp l r xt, lerp (-b) t yt)
        BaselineText -> return (0, 0)
      let P (newX, newY) = origin
      cairoTransf (moveOriginBy (refX - newX, newY - refY) mempty)
      C.showText str
      C.restore
