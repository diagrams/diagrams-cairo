module Diagrams.Backend.Cairo.List where

import Control.Applicative ((<$>))
import Control.Exception (bracket)

import Data.Colour.SRGB (Colour, sRGB)
import Data.Word (Word8)

import Diagrams.Prelude (Diagram, R2)
import Diagrams.Backend.Cairo (Cairo)
import Diagrams.Backend.Cairo.Ptr (renderPtr)

import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (peekArray)

-- | Render to a regular list of Colour values.

renderToList :: (Ord a, Floating a) =>
                  Int -> Int -> Diagram Cairo R2 -> IO [[Colour a]]
renderToList w h d =
  f 0 <$> bracket (renderPtr w h d) free (peekArray $ w*h*4)
 where
  f :: (Ord a, Floating a) => Int -> [Word8] -> [[Colour a]]
  f _ [] = []
  f n xs | n >= w = [] : f 0 xs
  f n (_:r:g:b:xs) = let c = sRGB (l r) (l g) (l b) in
    case f (n+1) xs of
      []    -> [[c]]
      cs:ys -> (c:cs) : ys

  f _ _ = error "renderToList: Internal format error"

  l :: (Ord a, Floating a) => Word8 -> a
  l n = fromIntegral n / fromIntegral (maxBound :: Word8)
