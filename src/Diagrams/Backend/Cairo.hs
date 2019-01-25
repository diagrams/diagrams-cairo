{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Cairo
-- Copyright   :  (c) 2011-2019 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-----------------------------------------------------------------------------
module Diagrams.Backend.Cairo where

import           Control.Exception               (try)
import           Control.Monad                   (when)
import           Control.Monad.IO.Class
import qualified Data.Array.MArray               as MA
import           Data.Bits                       (rotateL, (.&.))
import qualified Data.Foldable                   as F
import           Data.Hashable                   (Hashable (..))
import           Data.List                       (isSuffixOf)
import           Data.Maybe                      (fromMaybe, isJust)
import           Data.Typeable
import           Data.Word                       (Word32)
import           System.FilePath

import           Codec.Picture
import           Codec.Picture.Types             (convertImage, packPixel,
                                                  promoteImage)
import qualified Data.Vector.Storable            as SV
import           Foreign.ForeignPtr              (newForeignPtr)
import           Foreign.Marshal.Alloc           (finalizerFree)
import           Foreign.Marshal.Array           (callocArray)
import           Foreign.Ptr                     (Ptr, castPtr)
import           Graphics.Rendering.Cairo        (Format (..),
                                                  formatStrideForWidth,
                                                  renderWith,
                                                  withImageSurfaceForData)
import qualified Graphics.Rendering.Cairo        as C
import qualified Graphics.Rendering.Cairo.Matrix as CM
import qualified Graphics.Rendering.Pango        as P

import           Diagrams.Backend
import           Diagrams.Backend.Compile
import           Diagrams.Prelude                hiding (clip, opacity, output)
import           Diagrams.TwoD.Text              hiding (Font, font)
import           Diagrams.Types                  hiding (local)

import           Data.Word                       (Word8)



-- | This data declaration is simply used as a token to distinguish
--   the cairo backend: (1) when calling functions where the type
--   inference engine would otherwise have no way to know which
--   backend you wanted to use, and (2) as an argument to the
--   'Backend' and 'Renderable' type classes.
data Cairo = Cairo
  deriving (Eq,Ord,Read,Show,Typeable)

type instance V Cairo = V2
type instance N Cairo = Double

default2DAttrs :: Diagram V2 -> Diagram V2
default2DAttrs
  = lineWidth medium
  . lineTexture black

instance Backend Cairo where
  type Result  Cairo = C.Render ()
  data Options Cairo = CairoOptions
          { _cairoSizeSpec   :: SizeSpec V2 Int -- ^ The requested size of the output
          }
    deriving (Show)

  backendInfo = const cairoInfo
  renderDiaT opts dia = (sz, t2, r) where
    (sz, t2, dia') = adjustSize2D (opts^.sizeSpec) (default2DAttrs dia # reflectY)
    r = toRender t2 dia'

instance BackendBuild Cairo where
  saveDiagram' outPath opts d = do
    let (V2 w h,_,r) = renderDiaT opts d
        V2 w' h' = fmap round (V2 w h)
        f surf = C.renderWith surf r
    case takeExtension outPath of
      ".pdf" -> C.withPDFSurface outPath w h f
      ".svg" -> C.withSVGSurface outPath w h f
      ".ps"  -> C.withPSSurface outPath w h f
      ".png" ->
        C.withImageSurface C.FormatARGB32 w' h' $ \surface -> do
          f surface
          C.surfaceWriteToPNG surface outPath
      ext -> error $ "Unknown extension " <> show ext

  mkOptions sz = def & sizeSpec .~ sz
  sizeSpec     = cairoSizeSpec
  showOptions  = show

------------------------------------------------------------------------
-- Rendering
------------------------------------------------------------------------

toRender :: T2 Double -> Diagram V2 -> C.Render ()
toRender = foldDiaA renderPrim renderAnnot
  where
    renderPrim t2 attrs prim = case renderPrimitive t2 attrs prim of
      Just r  -> C.save >> r >> C.restore
      Nothing -> error $ "Unknown primitive"

renderPrimitive
  :: T2 Double -> Attributes -> Prim V2 Double -> Maybe (C.Render ())
renderPrimitive t2 attrs = \case
  Path_ path              -> Just $ renderPath t2 attrs path
  Text_ t                 -> Just $ renderText t2 attrs t
  ExternalImage_ w h path -> Just $ renderExternal t2 w h path
  EmbeddedImage_  i       -> Just $ renderEmbedded t2 i
  Prim _                  -> Nothing

renderAnnot :: Annotation V2 Double -> C.Render () -> C.Render ()
renderAnnot a r
  -- -- | Just x <- getAnnot _GroupOpacity a = C.save >> r >> C.paintWithAlpha x >> C.restore
  | Just p <- getAnnot _Clip         a = C.save >> clip (F.toList p) r >> C.restore
  | otherwise                          = r

clip :: [Path V2 Double] -> C.Render () -> C.Render ()
clip clips m = do
  F.for_ clips $ \path -> do
    cairoPath path
    C.clip
  m

instance Hashable (Options Cairo) where
  hashWithSalt s (CairoOptions sz)
    = s   `hashWithSalt`
      sz

instance Default (Options Cairo) where
  def = CairoOptions (dims2D 64 64)

cairoSizeSpec :: Lens' (Options Cairo) (SizeSpec V2 Int)
cairoSizeSpec = lens (\(CairoOptions {_cairoSizeSpec = s}) -> s)
                     (\o s -> o {_cairoSizeSpec = s})

cairoStyle :: Attributes -> C.Render ()
cairoStyle s = sequence_
  [ f _FillRule (C.setFillRule . fromFillRule)
  , f _LineWidth C.setLineWidth
  , f _LineCap (C.setLineCap . fromLineCap)
  , f _LineJoin (C.setLineJoin . fromLineJoin)
  , f _Dashing (\(Dashing ds offs) -> C.setDash ds offs)
  ]
  where
    f :: Typeable a => Getting r a r -> (r -> C.Render ()) -> C.Render ()
    f g r = mapM_ r (getAttr g s)

fromFontSlant :: FontSlant -> P.FontStyle
fromFontSlant FontSlantNormal   = P.StyleNormal
fromFontSlant FontSlantItalic   = P.StyleItalic
fromFontSlant FontSlantOblique  = P.StyleOblique

fromFontWeight :: FontWeight -> P.Weight
fromFontWeight FontWeightBold   = P.WeightBold
fromFontWeight _                = P.WeightNormal

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

renderPath
  :: T2 Double
  -> Attributes
  -> Path V2 Double
  -> C.Render ()
renderPath t attr p = do
  pathT t p
  let fill = getAttr _FillTexture attr
      line = getAttr _LineTexture attr
      o    = fromMaybe 1 $ getAttr _Opacity attr
  cairoStyle attr
  when (isJust fill) $ do
    setTexture o fill
    C.fillPreserve
  setTexture o line
  C.stroke

segment :: Segment V2 Double -> C.Render ()
segment = \case
  Linear (V2 x y)                        -> C.relLineTo x y
  Cubic (V2 x1 y1) (V2 x2 y2) (V2 x3 y3) -> C.relCurveTo x1 y1 x2 y2 x3 y3

pathT :: T2 Double -> Path V2 Double -> C.Render ()
pathT t p = C.newPath >> mapMOf_ each (trail . transform t) p

cairoPath :: Path V2 Double -> C.Render ()
cairoPath (Path path) = do
  C.newPath
  F.mapM_ trail path

trail :: Located (Trail V2 Double) -> C.Render ()
trail (Loc (P2 x y) t) = do
  C.moveTo x y
  case t of
    OpenTrail l            -> lineRender l
    ClosedTrail (Loop l c) -> do
      lineRender l
      close (negated $ offset l) c

lineRender :: Line V2 Double -> C.Render ()
lineRender = traverseOf_ segments segment

close :: V2 Double -> ClosingSegment V2 Double -> C.Render ()
close (V2 x3 y3) = \case
  LinearClosing      -> C.closePath
  CubicClosing (V2 x1 y1) (V2 x2 y2) ->
    C.relCurveTo x1 y1 x2 y2 x3 y3 >> C.closePath

-- Add a path to the Cairo context, without stroking or filling it.

addStop :: MonadIO m => C.Pattern -> GradientStop -> m ()
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
setTexture :: Double -> Maybe Texture -> C.Render ()
setTexture o (Just t) = case t of
  SC (SomeColor c) ->
    let (r,g,b,a) = colorToSRGBA c
    in  C.setSourceRGBA r g b (o*a)
  LG g ->
    let m = CM.Matrix a1 a2 b1 b2 c1 c2
        [[a1, a2], [b1, b2], [c1, c2]] = matrixHomRep (inv (g^.gradientTransform))
        (x0, y0) = unp2 (g^.gradientStart)
        (x1, y1) = unp2 (g^.gradientEnd)
    in  C.withLinearPattern x0 y0 x1 y1 $ \pat -> do
          mapM_ (addStop pat) (g^.gradientStops)
          C.patternSetMatrix pat m
          C.patternSetExtend pat (cairoSpreadMethod (g^.gradientSpreadMethod))
          C.setSource pat
  RG g ->
    let m = CM.Matrix a1 a2 b1 b2 c1 c2
        [[a1, a2], [b1, b2], [c1, c2]] = matrixHomRep (inv (g^.gradientTransform))
        (r0, r1) = (g^.gradientRadius0, g^.gradientRadius1)
        (x0', y0') = unp2 (g^.gradientCenter0)
        (x1', y1') = unp2 (g^.gradientCenter1)
        (x0, y0, x1, y1) = (x0' * (r1 - r0) / r1, y0' * (r1 - r0) / r1, x1' ,y1')
    in  C.withRadialPattern x0 y0 r0 x1 y1 r1 $ \pat -> do
          mapM_ (addStop pat) (g^.gradientStops)
          C.patternSetMatrix pat m
          C.patternSetExtend pat (cairoSpreadMethod (g^.gradientSpreadMethod))
          C.setSource pat
setTexture _ _ = return ()

-- Can only do PNG files at the moment...
renderExternal :: T2 Double -> Int -> Int -> FilePath -> C.Render ()
renderExternal tr w h file = do
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
        , " images in .png format. Ignoring <" ++ file ++ ">."
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

renderEmbedded :: T2 Double -> DynamicImage -> C.Render ()
renderEmbedded tr dImg = do
  let img@(Image w h _) = toImageRGBA8 dImg
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
if' = mapM_

-- instance Renderable (Text Double) Cairo where
renderText
  :: T2 Double
  -> Attributes
  -> Text Double
  -> C.Render ()
renderText t attrs txt = do
  let o = fromMaybe 1 (getAttr _Opacity attrs)
      t' = t <> scaling (1 / avgScale t)
  setTexture o $ getAttr _FillTexture attrs
  layout <- layoutStyledText t' attrs txt
  P.showLayout layout
  C.newPath

layoutStyledText
  :: T2 Double
  -> Attributes
  -> Text Double
  -> C.Render P.PangoLayout
layoutStyledText tt sty (Text al str) = do
  let tr = tt <> reflectionY
      ff = getAttr _Font sty
      fs = fromFontSlant <$> getAttr _FontSlant sty
      fw = fromFontWeight <$> getAttr _FontWeight sty
      size' = getAttr _FontSize sty
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

-- Rendering -----------------------------------------------------------

-- | Rasterise a 'C.Render' to a raw pointer.
rasterPtr :: Int -> Int -> Format -> C.Render () -> IO (Ptr Word8)
rasterPtr w h fmt r = do
  let stride = formatStrideForWidth fmt w
  b <- callocArray (stride * h)
  withImageSurfaceForData b fmt w h stride (`renderWith` r)
  pure (castPtr b)

-- | Rasterise a 'C.Render' to a JuicyPixels image.
rasterImage :: Int -> Int -> C.Render () -> IO (Image PixelRGBA8)
rasterImage w h r = do
  ptr  <- rasterPtr w h FormatARGB32 r
  fptr <- newForeignPtr finalizerFree ptr
  let vec = SV.unsafeFromForeignPtr0 fptr (w*h*4)
  pure (Image w h vec)

-- | Rasterise a 'C.Render' to a JuicyPixels image.
rasterDia :: SizeSpec V2 Int -> Diagram V2 -> IO (Image PixelRGBA8)
rasterDia sz = rasterDia' (def & sizeSpec .~ sz)

-- | Rasterise a 'C.Render' to a JuicyPixels image.
rasterDia' :: Options Cairo -> Diagram V2 -> IO (Image PixelRGBA8)
rasterDia' opts d = do
  let (V2 w h, _, r) = renderDiaT opts d
  rasterImage (ceiling w) (ceiling h) r
