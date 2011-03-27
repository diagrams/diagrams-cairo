import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Data.VectorSpace

type D = Diagram Cairo

sq :: D
sq = translate (-1/2,-1/2) $ b `atop` a
  where a = stroke . translate (1,0) $ arc (pi / 2) pi
        b = lw 0.5 . scale (1/2) . translate (1,1) $ square

phi :: Double
phi = (1 + sqrt 5) / 2

step :: D -> D -> D
step a b = besideAlign (-1,0) (0,1) a b'
  where b' = rotate (-pi/2) . scale (1/phi) $ b

besideAlign :: R2 -> R2 -> D -> D -> D
besideAlign u v a b = beside u b' a
  where b' = translate (v ^* d) b
        (Bounds bb) = bounds b
        (Bounds ba) = bounds a
        d = (ba v - bb v)

golden :: D
golden = foldr1 step (replicate 10 sq)

main :: IO ()
main = defaultMain golden