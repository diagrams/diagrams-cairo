{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

d1 = hcat' with {sep = 1} [square, mempty, square # fc blue]

d1' = hcat [square, strutX 1, mempty, strutX 1, square # fc red]

d2 = hcat' with {sep = 1} [square, square # fc green]

main = defaultMain (d1 ||| strutX 2 ||| d2 ||| strutX 2 ||| d1')