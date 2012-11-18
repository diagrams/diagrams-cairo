module Diagrams.Backend.Cairo.Ptr where

import Data.Word (Word8)

import Diagrams.Prelude (Diagram, R2, SizeSpec2D (..), renderDia)
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal

import Foreign.ForeignPtr.Safe (ForeignPtr, newForeignPtr)
import Foreign.Marshal.Alloc   (finalizerFree)
import Foreign.Marshal.Array   (mallocArray, pokeArray)
import Foreign.Ptr             (Ptr, castPtr)

import Graphics.Rendering.Cairo ( Format (..)
                                , formatStrideForWidth
                                , renderWith
                                , withImageSurfaceForData
                                )

-- | Render a diagram to a new buffer in memory, with the format ARGB32.

renderPtr :: Int -> Int -> Diagram Cairo R2 -> IO (Ptr Word8)
renderPtr w h d = do
  let stride = formatStrideForWidth FormatARGB32 w
      size   = stride * h
      opt    = CairoOptions
        { cairoSizeSpec     = Dims (fromIntegral w) (fromIntegral h)
        , cairoOutputType   = RenderOnly
        , cairoBypassAdjust = False
        , cairoFileName     = ""
        }
      (_, r) = renderDia Cairo opt d

  b <- mallocArray size
  pokeArray b (replicate size 0)
  withImageSurfaceForData b FormatARGB32 w h stride (`renderWith` r)

  return (castPtr b)

-- | Like 'renderPtr' but automatically garbage collected by Haskell.

renderForeignPtr :: Int -> Int -> Diagram Cairo R2 -> IO (ForeignPtr Word8)
renderForeignPtr w h d = renderPtr w h d >>= newForeignPtr finalizerFree
