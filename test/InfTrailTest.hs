import Diagrams.Prelude

import Diagrams.Backend.Cairo.CmdLine

t = polygon with { sides = 3 } # scale 10 # fc white # lc black # lw 0.2

ts = cat' (10,2) with { catMethod = Distrib } (replicate 6 t)

main = defaultMain ts