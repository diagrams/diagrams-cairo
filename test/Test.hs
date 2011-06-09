import Diagrams.Prelude

import Diagrams.Backend.Cairo.CmdLine

type D = Diagram Cairo R2

d = square `atop` circle
    # lc red
    # fc blue
    # lw 1

d' = d # withBounds (scale 2 circle :: D)

main = defaultMain d'