{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

main = defaultMain (circle 4 # withBounds (getBounds (unitSquare :: Diagram Cairo)))
