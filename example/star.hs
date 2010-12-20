import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.TwoD.Shapes
import Data.Colour.SRGB

star :: Diagram Cairo
star = rotate (pi/2) (createStar 5 2)

main = renderDia Cairo (CairoOptions "Star.pdf" (PDF (100.0,100.0))) star
