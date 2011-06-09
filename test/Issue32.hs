{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

d1 = hcat' with {sep = 1} [unitSquare, mempty, unitSquare # fc blue]

d1' = hcat [unitSquare, strutX 1, mempty, strutX 1, unitSquare # fc red]

d2 = hcat' with {sep = 1} [unitSquare, unitSquare # fc green]

main = defaultMain (d1 ||| strutX 2 ||| d2 ||| strutX 2 ||| d1')