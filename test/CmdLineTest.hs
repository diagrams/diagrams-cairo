import Diagrams.Prelude

import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.CmdLine

d = polygon with {sides=5}
d2 = polygon with {sides=17}

main = multiMain [("pent", d), ("foo", d2)]