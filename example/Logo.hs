{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import qualified Data.Colour as C

{-
d = mconcat . map alignBR
  $ [ dBody
    , rect 0.5 6.5 # fc black
    ]

blues = iterate (C.blend 0.1 white) blue
      # take 11 # reverse

dBody = mconcat . reverse . zipWith fc blues . take 11 . iterate (rotateBy (-1/20)) . rotateBy (-1/4) $ halfC

halfC = arc 0 (1/2 :: CircleFrac)
      # scale 1.8
      # stroke
      # lw 0
-}

i = (circle 1 === strutY 0.5 === roundedRect (2,4) 0.4)
    # lw 0.05
    # lc blue
    # fc yellow

sierpinski 1 = polygon with { sides = 3, orientation = OrientToX }
sierpinski n = s === (s ||| s)
  where s = sierpinski (n-1)

a = sierpinski 4
    # fc black
    # scale (1/2)

grid = verts # centerXY <> horiz # centerXY
  where verts = hcat' with {sep=0.5} $ replicate 20 (vrule 10)
        horiz = rotateBy (1/4) verts

g = grid
    # lc gray
    # rotateBy (-1/20)
    # clipBy p
    # withBounds (p :: Path R2)
  where p = polygon with {sides = 4, orientation = OrientToX} # scaleToX 5 # scaleToY 5

m = square 5.5 <>
    text "m"
      # fontSize 6 # italic # font "freeserif" # fc green

logo = (hcat' with {sep = 0.5} . map alignB $ [ i, a, g, m ])
       # centerXY

main = defaultMain (pad 1.1 logo)