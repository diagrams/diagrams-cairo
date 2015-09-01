{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ViewPatterns              #-}
{-# LANGUAGE ScopedTypeVariables       #-}

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

import           Diagrams.Core.Compile
import           Diagrams.Core.Transform

import           Diagrams.Prelude                hiding (font, opacity, view)
import           Diagrams.TwoD.Adjust            (adjustDia2D,
                                                  setDefault2DAttributes)
import           Diagrams.TwoD.Path              (Clip (Clip), getFillRule)
import           Diagrams.TwoD.Text              hiding (font)

import qualified Graphics.Rendering.Cairo        as C
import qualified Graphics.Rendering.Cairo.Matrix as CM
import qualified Graphics.Rendering.Pango        as P

import           Codec.Picture
import           Codec.Picture.Types             (convertImage, promoteImage, packPixel)


import           Control.Exception               (try)
import           Control.Monad                   (when)
import           Control.Monad.IO.Class
import qualified Control.Monad.StateStack        as SS
import           Control.Monad.Trans             (lift)
import qualified Data.Foldable                   as F
import           Data.Hashable                   (Hashable (..))
import           Data.List                       (isSuffixOf)
import           Data.Maybe                      (catMaybes, fromMaybe, isJust)
import           Data.Tree
import           Data.Typeable
import qualified Data.Vector.Storable            as VS
import qualified Data.Array.MArray               as MA
import           Data.Word                       (Word32)
import           Data.Bits                       (rotateL, (.&.))
import           GHC.Generics                    (Generic)

-- | This data declaration is simply used as a token to distinguish
--   the cairo backend: (1) when calling functions where the type
--   inference engine would otherwise have no way to know which
--   backend you wanted to use, and (2) as an argument to the
--   'Backend' and 'Renderable' type classes.
data Cairo = Cairo
  deriving (Eq,Ord,Read,Show,Typeable)

type B = Cairo

type instance V Cairo = V2
type instance N Cairo = Double

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
  deriving (Eq, Ord, Read, Show, Bounded, Enum, Typeable, Generic)

instance Hashable OutputType

-- | Custom state tracked in the 'RenderM' monad.
data CairoState
  = CairoState { _accumStyle :: Style V2 Double
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

instance Backend Cairo V2 Double where
  data Render  Cairo V2 Double = C (RenderM ())
  type Result  Cairo V2 Double = (IO (), C.Render ())
  data Options Cairo V2 Double = CairoOptions
          { _cairoFileName   :: String     -- ^ The name of the file you want generated
          , _cairoSizeSpec   :: SizeSpec V2 Double -- ^ The requested size of the output
          , _cairoOutputType :: OutputType -- ^ the output format and associated options
          , _cairoBypassAdjust  :: Bool    -- ^ Should the 'adjustDia' step be bypassed during rendering?
          }
    deriving (Show)

  renderRTree _ opts t = (renderIO, r)
    where
      r = runRenderM .runC . toRender $ t
      renderIO = do
        let surfaceF s = C.renderWith s r
            V2 w h = specToSize 1 (opts^.cairoSizeSpec)
        case opts^.cairoOutputType of
          PNG ->
            C.withImageSurface C.FormatARGB32 (round w) (round h) $ \surface -> do
              surfaceF surface
              C.surfaceWriteToPNG surface (opts^.cairoFileName)
          PS  -> C.withPSSurface  (opts^.cairoFileName) w h surfaceF
          PDF -> C.withPDFSurface (opts^.cairoFileName) w h surfaceF
          SVG -> C.withSVGSurface (opts^.cairoFileName) w h surfaceF
          RenderOnly -> return ()

  adjustDia c opts d = if _cairoBypassAdjust opts
                         then (opts, mempty, d # setDefault2DAttributes)
                         else let (opts', transformation, d') = adjustDia2D cairoSizeSpec c opts (d # reflectY)
                              in (opts', transformation <> reflectionY, d')

runC :: Render Cairo V2 Double -> RenderM ()
runC (C r) = r

instance Monoid (Render Cairo V2 Double) where
  mempty  = C $ return ()
  (C rd1) `mappend` (C rd2) = C (rd1 >> rd2)

instance Hashable (Options Cairo V2 Double) where
  hashWithSalt s (CairoOptions fn sz out adj)
    = s   `hashWithSalt`
      fn  `hashWithSalt`
      sz  `hashWithSalt`
      out `hashWithSalt`
      adj

toRender :: RTree Cairo V2 Double a -> Render Cairo V2 Double
toRender (Node (RPrim p) _) = render Cairo p
toRender (Node (RStyle sty) rs) = C $ do
  save
  cairoStyle sty
  accumStyle %= (<> sty)
  runC $ F.foldMap toRender rs
  restore
toRender (Node _ rs) = F.foldMap toRender rs

cairoFileName :: Lens' (Options Cairo V2 Double) String
cairoFileName = lens (\(CairoOptions {_cairoFileName = f}) -> f)
                     (\o f -> o {_cairoFileName = f})

cairoSizeSpec :: Lens' (Options Cairo V2 Double) (SizeSpec V2 Double)
cairoSizeSpec = lens (\(CairoOptions {_cairoSizeSpec = s}) -> s)
                     (\o s -> o {_cairoSizeSpec = s})

cairoOutputType :: Lens' (Options Cairo V2 Double) OutputType
cairoOutputType = lens (\(CairoOptions {_cairoOutputType = t}) -> t)
                     (\o t -> o {_cairoOutputType = t})

cairoBypassAdjust :: Lens' (Options Cairo V2 Double) Bool
cairoBypassAdjust = lens (\(CairoOptions {_cairoBypassAdjust = b}) -> b)
                     (\o b -> o {_cairoBypassAdjust = b})

-- | Render an object that the cairo backend knows how to render.
renderC :: (Renderable a Cairo, V a ~ V2, N a ~ Double) => a -> RenderM ()
renderC = runC . render Cairo

-- | Get an accumulated style attribute from the render monad state.
getStyleAttrib :: AttributeClass a => (a -> b) -> RenderM (Maybe b)
getStyleAttrib f = (fmap f . getAttr) <$> use accumStyle

-- | Handle those style attributes for which we can immediately emit
--   cairo instructions as we encounter them in the tree (clip, font
--   size, fill rule, line width, cap, join, and dashing).  Other
--   attributes (font face, slant, weight; fill color, stroke color,
--   opacity) must be accumulated.
cairoStyle :: Style v Double -> RenderM ()
cairoStyle s =
  sequence_
  . catMaybes $ [ handle clip
                , handle lFillRule
                , handle lWidth
                , handle lCap
                , handle lJoin
                , handle lDashing
                ]
  where handle :: AttributeClass a => (a -> RenderM ()) -> Maybe (RenderM ())
        handle f = f `fmap` getAttr s
        clip       = mapM_ (\p -> cairoPath p >> liftC C.clip) . op Clip
        lFillRule  = liftC . C.setFillRule . fromFillRule . getFillRule
        lWidth     = liftC . C.setLineWidth . getLineWidth
        lCap       = liftC . C.setLineCap . fromLineCap . getLineCap
        lJoin      = liftC . C.setLineJoin . fromLineJoin . getLineJoin
        lDashing (getDashing -> Dashing ds offs) =
          liftC $ C.setDash ds offs

fromFontSlant :: FontSlant -> P.FontStyle
fromFontSlant FontSlantNormal   = P.StyleNormal
fromFontSlant FontSlantItalic   = P.StyleItalic
fromFontSlant FontSlantOblique  = P.StyleOblique

fromFontWeight :: FontWeight -> P.Weight
fromFontWeight FontWeightNormal = P.WeightNormal
fromFontWeight FontWeightBold   = P.WeightBold

-- | Multiply the current transformation matrix by the given 2D
--   transformation.
cairoTransf :: T2 Double -> C.Render ()
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

instance Renderable (Segment Closed V2 Double) Cairo where
  render _ (Linear (OffsetClosed v)) = C . liftC $ uncurry C.relLineTo (unr2 v)
  render _ (Cubic (unr2 -> (x1,y1))
                  (unr2 -> (x2,y2))
                  (OffsetClosed (unr2 -> (x3,y3))))
    = C . liftC $ C.relCurveTo x1 y1 x2 y2 x3 y3

instance Renderable (Trail V2 Double) Cairo where
  render _ = withTrail renderLine renderLoop
    where
      renderLine ln = C $ do
        mapM_ renderC (lineSegments ln)

        -- remember that we saw a Line, so we will ignore fill attribute
        ignoreFill .= True

      renderLoop lp = C $ do
        case loopSegments lp of
          -- let closePath handle the last segment if it is linear
          (segs, Linear _) -> mapM_ renderC segs

          -- otherwise we have to draw it explicitly
          _ -> mapM_ renderC (lineSegments . cutLoop $ lp)

        liftC C.closePath

instance Renderable (Path V2 Double) Cairo where
  render _ p = C $ do
    cairoPath p
    f <- getStyleAttrib getFillTexture
    s <- getStyleAttrib getLineTexture
    ign <- use ignoreFill
    setTexture f
    when (isJust f && not ign) $ liftC C.fillPreserve
    setTexture s
    liftC C.stroke

-- Add a path to the Cairo context, without stroking or filling it.
cairoPath :: Path V2 Double -> RenderM ()
cairoPath (Path trs) = do
    liftC C.newPath
    ignoreFill .= False
    F.mapM_ renderTrail trs
  where
    renderTrail (viewLoc -> (unp2 -> p, tr)) = do
      liftC $ uncurry C.moveTo p
      renderC tr

addStop :: MonadIO m => C.Pattern -> GradientStop Double -> m ()
addStop p s = C.patternAddColorStopRGBA p (s^.stopFraction) r g b a
  where
    (r,g,b,a) = colorToSRGBA (s^.stopColor)

cairoSpreadMethod :: SpreadMethod -> C.Extend
cairoSpreadMethod GradPad = C.ExtendPad
cairoSpreadMethod GradReflect = C.ExtendReflect
cairoSpreadMethod GradRepeat = C.ExtendRepeat

-- XXX should handle opacity in a more straightforward way, using
-- cairo's built-in support for transparency?  See also
-- https://github.com/diagrams/diagrams-cairo/issues/15 .
setTexture :: Maybe (Texture Double) -> RenderM ()
setTexture Nothing = return ()
setTexture (Just (SC (SomeColor c))) = do
    o <- fromMaybe 1 <$> getStyleAttrib getOpacity
    liftC (C.setSourceRGBA r g b (o*a))
  where (r,g,b,a) = colorToSRGBA c
setTexture (Just (LG g)) = liftC $
    C.withLinearPattern x0 y0 x1 y1 $ \pat -> do
      mapM_ (addStop pat) (g^.lGradStops)
      C.patternSetMatrix pat m
      C.patternSetExtend pat (cairoSpreadMethod (g^.lGradSpreadMethod))
      C.setSource pat
  where
    m = CM.Matrix a1 a2 b1 b2 c1 c2
    [[a1, a2], [b1, b2], [c1, c2]] = matrixHomRep (inv (g^.lGradTrans))
    (x0, y0) = unp2 (g^.lGradStart)
    (x1, y1) = unp2 (g^.lGradEnd)
setTexture (Just (RG g)) = liftC $
    C.withRadialPattern x0 y0 r0 x1 y1 r1 $ \pat -> do
      mapM_ (addStop pat) (g^.rGradStops)
      C.patternSetMatrix pat m
      C.patternSetExtend pat (cairoSpreadMethod (g^.rGradSpreadMethod))
      C.setSource pat
  where
    m = CM.Matrix a1 a2 b1 b2 c1 c2
    [[a1, a2], [b1, b2], [c1, c2]] = matrixHomRep (inv (g^.rGradTrans))
    (r0, r1) = (g^.rGradRadius0, g^.rGradRadius1)
    (x0', y0') = unp2 (g^.rGradCenter0)
    (x1', y1') = unp2 (g^.rGradCenter1)
    (x0, y0, x1, y1) = (x0' * (r1 - r0) / r1, y0' * (r1 - r0) / r1, x1' ,y1')

-- Can only do PNG files at the moment...
instance Renderable (DImage Double External) Cairo where
  render _ (DImage path w h tr) = C . liftC $ do
    let ImageRef file = path
    if ".png" `isSuffixOf` file
      then do
        C.save
        cairoTransf (tr <> reflectionY)
        pngSurfChk <- liftIO (try $ C.imageSurfaceCreateFromPNG file
                              :: IO (Either IOError C.Surface))
        case pngSurfChk of
          Right pngSurf -> do
            w' <- C.imageSurfaceGetWidth pngSurf
            h' <- C.imageSurfaceGetHeight pngSurf
            let sz = fromIntegral <$> dims2D w h
            cairoTransf $ requiredScaling sz (fromIntegral <$> V2 w' h')
            C.setSourceSurface pngSurf (-fromIntegral w' / 2)
                                       (-fromIntegral h' / 2)
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

-- Copied from Rasterific backend. This function should probably be in JuicyPixels!
toImageRGBA8 :: DynamicImage -> Image PixelRGBA8
toImageRGBA8 (ImageRGBA8 i)  = i
toImageRGBA8 (ImageRGB8 i)   = promoteImage i
toImageRGBA8 (ImageYCbCr8 i) = promoteImage (convertImage i :: Image PixelRGB8)
toImageRGBA8 (ImageY8 i)     = promoteImage i
toImageRGBA8 (ImageYA8 i)    = promoteImage i
toImageRGBA8 (ImageCMYK8 i)  = promoteImage (convertImage i :: Image PixelRGB8)
toImageRGBA8 _               = error "Unsupported Pixel type"

instance Renderable (DImage Double Embedded) Cairo where
  -- render _ (DImage path w h tr) =
  render _ (DImage iD _w _h tr) = C . liftC $ do
     C.save
     cairoTransf (tr <> reflectionY)
     
     let fmt = C.FormatARGB32
     dataSurf <- liftIO $ C.createImageSurface fmt w h
     
     surData :: C.SurfaceData Int Word32
             <- liftIO $ C.imageSurfaceGetPixels dataSurf
     
     stride <- C.imageSurfaceGetStride dataSurf
     
     _ <- forMOf imageIPixels img $ \(x, y, px) -> do
        let p = y * (stride`div`4) + x
        liftIO . MA.writeArray surData p $ toARGB px
        return px
     
     C.surfaceMarkDirty dataSurf
     
     w' <- C.imageSurfaceGetWidth dataSurf
     h' <- C.imageSurfaceGetHeight dataSurf
     let sz = fromIntegral <$> dims2D w h
     cairoTransf $ requiredScaling sz (fromIntegral <$> V2 w' h')
     C.setSourceSurface dataSurf (-fromIntegral w' / 2)
                                 (-fromIntegral h' / 2)
     
     C.paint
     C.restore
    where
      ImageRaster dImg = iD
      img@(Image w h _) = toImageRGBA8 dImg
      

{-# INLINE toARGB #-}
-- Actually the name should be toBGRA, since that's the component order used by Cairo.
-- Really, what's happening here is just a swap of the R and B channels.
-- It seems a lot like this is dependent on endianness; perhaps we should handle this...
toARGB :: PixelRGBA8 -> Word32
toARGB px = ga + rotateL rb 16
 where rgba = packPixel px
       rb = rgba .&. 0x00FF00FF
       ga = rgba .&. 0xFF00FF00

if' :: Monad m => (a -> m ()) -> Maybe a -> m ()
if' = maybe (return ())

instance Renderable (Text Double) Cairo where
  render _ txt = C $ do
    save
    setTexture =<< getStyleAttrib getFillTexture
    sty <- use accumStyle
    layout <- liftC $ layoutStyledText sty txt
    -- Uncomment the lines below to draw a rectangle at the extent of each Text
    -- let (w, h) = unr2 $ ref ^* 2   -- XXX Debugging
    -- cairoPath $ rect w h           -- XXX Debugging
    liftC $ do
      -- C.setLineWidth 0.5 -- XXX Debugging
      -- C.stroke -- XXX Debugging
      -- C.newPath -- XXX Debugging
      P.showLayout layout
      C.newPath
    restore

layoutStyledText :: Style V2 Double -> Text Double -> C.Render P.PangoLayout
layoutStyledText sty (Text tt al str) =
  let tr = tt <> reflectionY
      styAttr :: AttributeClass a => (a -> b) -> Maybe b
      styAttr f = fmap f $ getAttr sty
      ff = styAttr getFont
      fs = styAttr fromFontSlant
      fw = styAttr fromFontWeight
      size' = styAttr getFontSize
  in do
    cairoTransf tr -- non-uniform scale
    layout <- P.createLayout str
    -- set font, including size
    liftIO $ do
      font <- P.fontDescriptionNew
      if' (P.fontDescriptionSetFamily font) ff
      if' (P.fontDescriptionSetStyle font) fs
      if' (P.fontDescriptionSetWeight font) fw
      if' (P.fontDescriptionSetSize font) size'
      P.layoutSetFontDescription layout $ Just font
    -- geometric translation
    ref <- liftIO $ case al of
      BoxAlignedText xt yt -> do
        (_,P.PangoRectangle _ _ w h) <- P.layoutGetExtents layout
        return $ r2 (w * xt, h * (1 - yt))
      BaselineText -> do
        baseline <- P.layoutIterGetBaseline =<< P.layoutGetIter layout
        return $ r2 (0, baseline)
    let t = moveOriginBy ref mempty :: T2 Double
    cairoTransf t
    P.updateLayout layout
    return layout
