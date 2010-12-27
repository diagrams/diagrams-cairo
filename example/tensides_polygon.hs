import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.TwoD.Shapes
import Data.Colour.SRGB
p :: Diagram Cairo
p = fc (sRGB 0.4 0.4 0.4) (polygon with {sides=10})

bigP:: Diagram Cairo
bigP = scale 2 p

twoP :: Diagram Cairo
twoP = beside (1,0) bigP p

main = renderDia Cairo (CairoOptions "Tensides_polygon.png" (PNG (400, 300))) twoP
