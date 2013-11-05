{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Cairo.Internal
-- Copyright   :  (c) 2011 Diagrams-cairo team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This module contains the internal implementation guts of the
-- diagrams cairo backend.  If you want to see how the cairo backend
-- works under the hood, you are in the right place (try clicking on
-- the \"Source\" links).  (Guts under the hood, what an awful mixed
-- metaphor.)  If you know what you are doing and really want access
-- to the internals of the implementation, you are also in the right
-- place.  Otherwise, you should have no need of this module; import
-- "Diagrams.Backend.Cairo.CmdLine" or "Diagrams.Backend.Cairo"
-- instead.
--
-- The one exception is that this module may have to be imported
-- sometimes to work around an apparent bug in certain versions of
-- GHC, which results in a \"not in scope\" error for 'CairoOptions'.
--
-- The types of all the @fromX@ functions look funny in the Haddock
-- output, which displays them like @Type -> Type@.  In fact they are
-- all of the form @Type -> Graphics.Rendering.Cairo.Type@, /i.e./
-- they convert from a diagrams type to a cairo type of the same name.
-----------------------------------------------------------------------------
module Diagrams.Backend.Cairo.Internal where

import           Diagrams.Core.Compile           (toRTree)
import           Diagrams.Core.Transform

import           Diagrams.Prelude                hiding (opacity, view)
import           Diagrams.TwoD.Adjust            (adjustDia2D,
                                                  setDefault2DAttributes)
import           Diagrams.TwoD.Image
import           Diagrams.TwoD.Path              (Clip (Clip), getFillRule)
import           Diagrams.TwoD.Size              (requiredScaleT)
import           Diagrams.TwoD.Text

import qualified Graphics.Rendering.Cairo        as C
import qualified Graphics.Rendering.Cairo.Matrix as CM

import           Control.Monad                   (when)
import qualified Control.Monad.StateStack        as SS
import           Control.Monad.Trans             (lift, liftIO)
import           Data.Default.Class
import           Data.List                       (isSuffixOf)
import           Data.Maybe                      (catMaybes, fromMaybe, isJust)
import           Data.Tree

import           Control.Lens                    hiding (transform, ( # ))

import           Control.Exception               (try)

import qualified Data.Foldable                   as F

import           Data.Typeable

-- | This data declaration is simply used as a token to distinguish
--   the cairo backend: (1) when calling functions where the type
--   inference engine would otherwise have no way to know which
--   backend you wanted to use, and (2) as an argument to the
--   'Backend' and 'Renderable' type classes.
data Cairo = Cairo
  deriving (Eq,Ord,Read,Show,Typeable)

type B = Cairo

-- | Output types supported by cairo, including four different file
--   types (PNG, PS, PDF, SVG).  If you want to output directly to GTK
--   windows, see the @diagrams-gtk@ package.
data OutputType =
    PNG         -- ^ Portable Network Graphics output.
  | PS          -- ^ PostScript output
  | PDF         -- ^ Portable Document Format output.
  | SVG         -- ^ Scalable Vector Graphics output.
  | RenderOnly  -- ^ Don't output any file; the returned @IO ()@
                --   action will do nothing, but the @Render ()@
                --   action can be used (/e.g./ to draw to a Gtk
                --   window; see the @diagrams-gtk@ package).
  deriving (Eq, Ord, Read, Show, Bounded, Enum, Typeable)

-- | Custom state tracked in the 'RenderM' monad.
data CairoState
  = CairoState { _accumStyle :: Style R2
                 -- ^ The current accumulated style.
               , _ignoreFill :: Bool
                 -- ^ Whether or not we saw any lines in the most
                 --   recent path (as opposed to loops).  If we did,
                 --   we should ignore any fill attribute.
                 --   diagrams-lib separates lines and loops into
                 --   separate path primitives so we don't have to
                 --   worry about seeing them together in the same
                 --   path.
               }

$(makeLenses ''CairoState)

instance Default CairoState where
  def = CairoState
        { _accumStyle       = mempty
        , _ignoreFill       = False
        }

-- | The custom monad in which intermediate drawing options take
--   place; 'Graphics.Rendering.Cairo.Render' is cairo's own rendering
--   monad.
type RenderM a = SS.StateStackT CairoState C.Render a

liftC :: C.Render a -> RenderM a
liftC = lift

runRenderM :: RenderM a -> C.Render a
runRenderM = flip SS.evalStateStackT def

-- | Push the current context onto a stack.
save :: RenderM ()
save =  SS.save >> liftC C.save

-- | Restore the context from a stack.
restore :: RenderM ()
restore = liftC C.restore >> SS.restore

instance Backend Cairo R2 where
  data Render  Cairo R2 = C (RenderM ())
  type Result  Cairo R2 = (IO (), C.Render ())
  data Options Cairo R2 = CairoOptions
          { _cairoFileName   :: String     -- ^ The name of the file you want generated
          , _cairoSizeSpec   :: SizeSpec2D -- ^ The requested size of the output
          , _cairoOutputType :: OutputType -- ^ the output format and associated options
          , _cairoBypassAdjust  :: Bool    -- ^ Should the 'adjustDia' step be bypassed during rendering?
          }
    deriving Show

  doRender _ (CairoOptions file size out _) (C r) = (renderIO, r')
    where r' = runRenderM r
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
              PNG ->
                C.withImageSurface C.FormatARGB32 (round w) (round h) $ \surface -> do
                  surfaceF surface
                  C.surfaceWriteToPNG surface file
              PS  -> C.withPSSurface  file w h surfaceF
              PDF -> C.withPDFSurface file w h surfaceF
              SVG -> C.withSVGSurface file w h surfaceF
              RenderOnly -> return ()

  -- renderData :: Monoid' m => b -> QDiagram b v m -> Render b v
  renderData _ = renderRTree . toRTree

  adjustDia c opts d = if _cairoBypassAdjust opts
                         then (opts, d # setDefault2DAttributes)
                         else adjustDia2D _cairoSizeSpec
                                          setCairoSizeSpec
                                          c opts (d # reflectY)
    where setCairoSizeSpec sz o = o { _cairoSizeSpec = sz }

runC :: Render Cairo R2 -> RenderM ()
runC (C r) = r

instance Monoid (Render Cairo R2) where
  mempty  = C $ return ()
  (C rd1) `mappend` (C rd2) = C (rd1 >> rd2)

renderRTree :: RTree Cairo R2 a -> Render Cairo R2
renderRTree (Node (RPrim accTr p) _) = render Cairo (transform accTr p)
renderRTree (Node (RStyle sty) ts)   = C $ do
  save
  cairoStyle sty
  accumStyle %= (<> sty)
  runC $ F.foldMap renderRTree ts
  restore
renderRTree (Node (RFrozenTr tr) ts) = C $ do
  save
  liftC $ cairoTransf tr
  runC $ F.foldMap renderRTree ts
  restore
renderRTree (Node _ ts)              = F.foldMap renderRTree ts

cairoFileName :: Lens' (Options Cairo R2) String
cairoFileName = lens (\(CairoOptions {_cairoFileName = f}) -> f)
                     (\o f -> o {_cairoFileName = f})

cairoSizeSpec :: Lens' (Options Cairo R2) SizeSpec2D
cairoSizeSpec = lens (\(CairoOptions {_cairoSizeSpec = s}) -> s)
                     (\o s -> o {_cairoSizeSpec = s})

cairoOutputType :: Lens' (Options Cairo R2) OutputType
cairoOutputType = lens (\(CairoOptions {_cairoOutputType = t}) -> t)
                     (\o t -> o {_cairoOutputType = t})

cairoBypassAdjust :: Lens' (Options Cairo R2) Bool
cairoBypassAdjust = lens (\(CairoOptions {_cairoBypassAdjust = b}) -> b)
                     (\o b -> o {_cairoBypassAdjust = b})

-- | Render an object that the cairo backend knows how to render.
renderC :: (Renderable a Cairo, V a ~ R2) => a -> RenderM ()
renderC = runC . render Cairo

-- | Get an accumulated style attribute from the render monad state.
getStyleAttrib :: AttributeClass a => (a -> b) -> RenderM (Maybe b)
getStyleAttrib f = (fmap f . getAttr) <$> use accumStyle

-- | Handle those style attributes for which we can immediately emit
--   cairo instructions as we encounter them in the tree (clip, font
--   size, fill rule, line width, cap, join, and dashing).  Other
--   attributes (font face, slant, weight; fill color, stroke color,
--   opacity) must be accumulated.
cairoStyle :: Style v -> RenderM ()
cairoStyle s =
  sequence_
  . catMaybes $ [ handle clip
                , handle fSize
                , handle lFillRule
                , handle lWidth
                , handle lCap
                , handle lJoin
                , handle lDashing
                ]
  where handle :: AttributeClass a => (a -> RenderM ()) -> Maybe (RenderM ())
        handle f = f `fmap` getAttr s
        clip       = mapM_ (\p -> cairoPath p >> liftC C.clip) . op Clip
        fSize      = liftC . C.setFontSize . getFontSize
        lFillRule  = liftC . C.setFillRule . fromFillRule . getFillRule
        lWidth     = liftC . C.setLineWidth . getLineWidth
        lCap       = liftC . C.setLineCap . fromLineCap . getLineCap
        lJoin      = liftC . C.setLineJoin . fromLineJoin . getLineJoin
        lDashing (getDashing -> Dashing ds offs) =
          liftC $ C.setDash ds offs

fromFontSlant :: FontSlant -> C.FontSlant
fromFontSlant FontSlantNormal   = C.FontSlantNormal
fromFontSlant FontSlantItalic   = C.FontSlantItalic
fromFontSlant FontSlantOblique  = C.FontSlantOblique

fromFontWeight :: FontWeight -> C.FontWeight
fromFontWeight FontWeightNormal = C.FontWeightNormal
fromFontWeight FontWeightBold   = C.FontWeightBold

-- | Apply the opacity from a style to a given color.
applyOpacity :: Color c => c -> Style v -> AlphaColour Double
applyOpacity c s = dissolve (fromMaybe 1 $ getOpacity <$> getAttr s) (toAlphaColour c)

-- | Multiply the current transformation matrix by the given 2D
--   transformation.
cairoTransf :: T2 -> C.Render ()
cairoTransf t = C.transform m
  where m = CM.Matrix a1 a2 b1 b2 c1 c2
        (unr2 -> (a1,a2)) = apply t unitX
        (unr2 -> (b1,b2)) = apply t unitY
        (unr2 -> (c1,c2)) = transl t

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

instance Renderable (Segment Closed R2) Cairo where
  render _ (Linear (OffsetClosed v)) = C . liftC $ uncurry C.relLineTo (unr2 v)
  render _ (Cubic (unr2 -> (x1,y1))
                  (unr2 -> (x2,y2))
                  (OffsetClosed (unr2 -> (x3,y3))))
    = C . liftC $ C.relCurveTo x1 y1 x2 y2 x3 y3

instance Renderable (Trail R2) Cairo where
  render _ t = flip withLine t $ renderT . lineSegments
    where
      renderT segs =
        C $ do
          mapM_ renderC segs
          liftC $ when (isLoop t) C.closePath

          when (isLine t) (ignoreFill .= True)
            -- remember that we saw a Line, so we will ignore fill attribute

instance Renderable (Path R2) Cairo where
  render _ p = C $ do
    cairoPath p

    f <- getStyleAttrib (toAlphaColour . getFillColor)
    s <- getStyleAttrib (toAlphaColour . getLineColor)
    ign <- use ignoreFill
    setSourceColor f
    when (isJust f && not ign) $ liftC C.fillPreserve
    setSourceColor s
    liftC C.stroke

-- Add a path to the Cairo context, without stroking or filling it.
cairoPath :: Path R2 -> RenderM ()
cairoPath (Path trs) = do
    liftC C.newPath
    ignoreFill .= False
    F.mapM_ renderTrail trs
  where
    renderTrail (viewLoc -> (unp2 -> p, tr)) = do
      liftC $ uncurry C.moveTo p
      renderC tr

-- XXX should handle opacity in a more straightforward way, using
-- cairo's built-in support for transparency?  See also
-- https://github.com/diagrams/diagrams-cairo/issues/15 .
setSourceColor :: Maybe (AlphaColour Double) -> RenderM ()
setSourceColor Nothing  = return ()
setSourceColor (Just c) = do
    o <- fromMaybe 1 <$> getStyleAttrib getOpacity
    liftC (C.setSourceRGBA r g b (o*a))
  where (r,g,b,a) = colorToSRGBA c

-- Can only do PNG files at the moment...
instance Renderable Image Cairo where
  render _ (Image file sz tr) = C . liftC $ do
    if ".png" `isSuffixOf` file
      then do
        C.save
        cairoTransf (tr <> reflectionY)
        pngSurfChk <- liftIO (try $ C.imageSurfaceCreateFromPNG file
                              :: IO (Either IOError C.Surface))
        case pngSurfChk of
          Right pngSurf -> do
            w <- C.imageSurfaceGetWidth pngSurf
            h <- C.imageSurfaceGetHeight pngSurf
            cairoTransf $ requiredScaleT sz (fromIntegral w, fromIntegral h)
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
    ff <- fromMaybe "" <$> getStyleAttrib getFont
    fs <- fromMaybe C.FontSlantNormal <$> getStyleAttrib (fromFontSlant . getFontSlant)
    fw <- fromMaybe C.FontWeightNormal <$> getStyleAttrib (fromFontWeight . getFontWeight)
    f  <- getStyleAttrib (toAlphaColour . getFillColor)
    save
    setSourceColor f
    liftC $ do
      C.selectFontFace ff fs fw
      -- XXX should use reflection font matrix here instead?
      cairoTransf (tr <> reflectionY)
      (refX, refY) <- case al of
        BoxAlignedText xt yt -> do
          tExt <- C.textExtents str
          fExt <- C.fontExtents
          let l = C.textExtentsXbearing tExt
              r = C.textExtentsXadvance tExt
              b = C.fontExtentsDescent  fExt
              t = C.fontExtentsAscent   fExt
          return (lerp l r xt, lerp (-b) t yt)
        BaselineText -> return (0, 0)
      cairoTransf (moveOriginBy (r2 (refX, -refY)) mempty)
      C.showText str
      C.newPath
    restore
