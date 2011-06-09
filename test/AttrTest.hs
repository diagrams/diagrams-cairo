import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

b1 = square 20
--            # lc red
--            # fc blue
            # lw 0.002

main = defaultMain (pad 1.1 b1)
