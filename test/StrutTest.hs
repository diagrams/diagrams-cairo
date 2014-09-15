import Diagrams.Prelude

import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.CmdLine

dia :: Diagram Cairo V2 Double
dia = beside (1,0) unitCircle (beside (1,0) (strut (1,0)) unitCircle)

main = defaultMain dia