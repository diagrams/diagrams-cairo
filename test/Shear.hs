{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

s = square 1

s' = s # shearY 2 # scale 0.8

main = defaultMain (s ||| s' ||| s' # rotateBy (1/20) === s' === rotateBy (1/20) s')