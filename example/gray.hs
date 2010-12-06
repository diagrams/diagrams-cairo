import Diagrams.Prelude
import Diagrams.Backend.Cairo

import Data.List.Split      (chunk)
import Data.Maybe           (catMaybes)
import Control.Applicative
import Data.Monoid          (mconcat)
import Data.List            (transpose)

gray 0 = [[]]
gray n = map (False:) g ++ map (True:) (reverse g)
  where g = gray (n-1)

rings n = mkRingsDia . map ringOffsets . transpose . gray $ n
  where ringOffsets :: [Bool] -> [(Angle, Angle)]
        ringOffsets = map l2t . chunk 2 . findEdges . zip [0,2*pi/(2^n)..2*pi]
        l2t [x,y] = (x,y)
        l2t [x]   = (x,2*pi)

findEdges :: Eq a => [(Angle, a)] -> [Angle]
findEdges = catMaybes . (zipWith edge <*> tail)
  where edge (_,c1) (a,c2) | c1 /= c2  = Just a
                           | otherwise = Nothing

mkRingsDia :: [[(Angle, Angle)]] -> Diagram Cairo
mkRingsDia = mconcat . zipWith mkRingDia [1,1.5..]
  where mkRingDia r = mconcat . map (stroke . scale r . uncurry arc)

main = renderDia Cairo (CairoOptions "gray.pdf" $ PDF (400, 400)) (rings 10)