import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.TwoD.Shapes
import Data.Colour.SRGB

p :: Diagram Cairo
p = polygon with {sides=3, orientation = OrientToX}

main = renderDia Cairo (CairoOptions "triangle.png" (PNG (400, 300))) p
