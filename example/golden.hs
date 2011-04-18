{-# LANGUAGE NoMonomorphismRestriction #-}
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Data.VectorSpace

type D = Diagram Cairo R2

sq :: D
sq = stroke (arc (pi / 2) pi) `atop` translate (-0.5, 0.5) square

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
golden = foldr1 step (replicate 10 sq) # lw 0.005

main :: IO ()
main = defaultMain golden