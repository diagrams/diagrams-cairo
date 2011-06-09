import Diagrams.Prelude

import Diagrams.Backend.Cairo.CmdLine

type D = Diagram Cairo R2

d = unitSquare `atop` unitCircle
    # lc red
    # fc blue
    # lw 1

d' = d # withBounds (circle 2 :: D)

main = defaultMain d'