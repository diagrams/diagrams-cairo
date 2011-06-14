{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

daisyD = image "daisy-duck.png" 400 400

daisy = mconcat . take 20 . iterate (rotateBy (1/15)) . alignB $ daisyD

main = defaultMain (pad 1.1 daisy)