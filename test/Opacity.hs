{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

d = (unitCircle # lc green # fc blue) <> (unitSquare # fc red)

main = defaultMain (d # opacity 0.8 # opacity 0.8 # opacity 0.8 # opacity 0.6)