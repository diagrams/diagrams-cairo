import Diagrams.Prelude

import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.CmdLine

dia :: Diagram Cairo
dia = beside (1,0) circle (beside (1,0) (strut (1,0)) circle)

main = defaultMain dia