{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

d = unitSquare
    # fc red
    # clipBy clipPath

clipPath :: Path V2 Double
clipPath = polygonPath with {sides = 4} # scale (1/sqrt 2)

main = defaultMain (pad 1.1 d)