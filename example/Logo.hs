{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

i = (circle 1 === strutY 0.5 === roundedRect (2,4) 0.4)
    # lw 0.05
    # lc blue
    # fc yellow

sierpinski 1 = polygon with { sides = 3, orientation = OrientToX }
sierpinski n = s === (s ||| s)
  where s = sierpinski (n-1)

a = sierpinski 3
    # fc black

grid = verts # centerXY <> horiz # centerXY
  where verts = hcat' with {sep=0.5} $ replicate 20 (vrule 10)
        horiz = rotateBy (1/4) verts

g = grid
    # lc gray
    # rotateBy (-1/20)
    # clipBy p
    # withBounds (p :: Path R2)
  where p = polygonPath with {sides = 4, orientation = OrientToX} # scaleToX 5 # scaleToY 5

m = square 5.5 <>
    text "m"
      # fontSize 6 # italic # font "freeserif" # fc green

logo = (hcat' with {sep = 0.5} . map alignB $ [ i, a, m, g ])
       # centerXY

main = defaultMain (pad 1.1 logo)