{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

d = square <> reflectAbout (P (1/2,1/2)) (-2,-1) square

main = defaultMain (pad 1.1 d)