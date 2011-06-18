{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Data.Colour

d = (roundedRect (5,2) 0.3 <> polygon with {sides=3})
    # lw 0.05
    # lc blue
    # fcA (green `withOpacity` 0.5)

main = defaultMain (pad 1.1 d)