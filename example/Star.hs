{-# LANGUAGE NoMonomorphismRestriction, TupleSections #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

axes n = h <> v
  where p = fromOffsets . replicate n
        h = stroke' with {vertexNames = [map ("x",) [0..n]]} (p unitX)
        v = stroke' with {vertexNames = [map ("y",) [0..n]]} (p unitY)

connect n i = withAName ("x",i) $ \x ->
              withAName ("y", n - i) $ \y ->
                drawConnect [x,y]
  where drawConnect = atop . fromVertices

pic n = applyAll (map (connect n) [0..n]) (axes n) # centerXY # lw 0.05

d n = half === rotateBy (1/2) half
  where half = rotateBy (1/4) (pic n) ||| pic n

main = defaultMain (pad 1.1 $ d 20 # lc blue)
