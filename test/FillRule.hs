{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

path = close . fromOffsets $ [(0,10), (3,0), (0, -8), (2,0), (0,2), (-4,0), (0, 3)
                             ,(9, 0), (0, -7)]

d = stroke path
  # fc red
  # lw 0.05
  # fillRule EvenOdd 

main = defaultMain (pad 1.1 d)