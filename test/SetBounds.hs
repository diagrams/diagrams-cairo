{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

main = defaultMain
         (circle 2 |||
          square 1.5 # rotateBy (1/8) # withBounds (square 1 :: Diagram Cairo))
