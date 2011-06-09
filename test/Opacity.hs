{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

d = (circle # lc green # fc blue) <> (square # fc red)

main = defaultMain (d # fade 0.8 # fade 0.8 # fade 0.8 # fade 0.6)