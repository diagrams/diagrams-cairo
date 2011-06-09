{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

main = defaultMain (circle # withBounds (getBounds (square :: Diagram Cairo R2) # scale 4))