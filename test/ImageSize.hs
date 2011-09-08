{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

d = image "daisy-duck.png" 450 450

main = defaultMain (pad 1.1 $ d)