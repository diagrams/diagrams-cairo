import Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Transform

import Diagrams.TwoD
import Diagrams.Combinators
import Diagrams.Backend.Cairo

d :: Diagram Cairo
d = translate (30,30) $ scale 10 (beside (2,1) box box `atop` box)

main = renderDia Cairo (CairoOptions "test.pdf" (PDF (100.0, 100.0))) d
