{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

d = unitSquare <> reflectAbout (P (1/2,1/2)) (-2,-1) unitSquare

main = defaultMain (pad 1.1 d)