{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

t = text "Yay, text!"
    # font "dejavu lgc serif"
    # italic
    # bold

d =  t
--  <> t # alignText alignL # fc yellow
--  <> t # alignText alignT # fc green
--  <> t # alignText alignB # fc blue
--  <> t # alignText alignR # fc red
--  <> t # alignText (alignTL . alignTL) # fc orange
  <> rect 15 3 # lw 0 # showOrigin

d2 = decorateTrail (fromVertices (iterate (translateY (-0.8)) origin))
   $ zipWith fontSize [1,0.9..0.3] (repeat t)

main = defaultMain (d === d2)