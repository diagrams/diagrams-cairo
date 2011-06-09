import Diagrams.Prelude

import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.CmdLine

dia :: Diagram Cairo R2
dia = beside (1,0) unitCircle (beside (1,0) (strut (1,0)) unitCircle)

main = defaultMain dia