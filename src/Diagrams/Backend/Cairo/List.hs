module Diagrams.Backend.Cairo.List where

import Control.Applicative ((<$>))
import Control.Exception (bracket)

import Data.Colour
import Data.Colour.SRGB (sRGB)
import Data.Word (Word8)

import Diagrams.Prelude (Diagram, R2)
import Diagrams.Backend.Cairo (Cairo)
import Diagrams.Backend.Cairo.Ptr (renderPtr)

import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (peekArray)

-- | Render to a regular list of Colour values.

renderToList :: (Ord a, Floating a) =>
                  Int -> Int -> Diagram Cairo R2 -> IO [[AlphaColour a]]
renderToList w h d =
  f 0 <$> bracket (renderPtr w h d) free (peekArray $ w*h*4)
 where
  f :: (Ord a, Floating a) => Int -> [Word8] -> [[AlphaColour a]]
  f _ [] = []
  f n xs | n >= w = [] : f 0 xs
  f n (g:b:r:a:xs) =
    let l n = fromIntegral n / fromIntegral a
        c   = sRGB (l r) (l g) (l b) `withOpacity` (fromIntegral a / 255)

    in case f (n+1) xs of
      []    -> [[c]]
      cs:ys -> (c:cs) : ys

  f _ _ = error "renderToList: Internal format error"
