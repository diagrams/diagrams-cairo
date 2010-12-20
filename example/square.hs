import Diagrams.Prelude 
import Diagrams.Backend.Cairo
import Diagrams.TwoD.Shapes
import Data.Colour.SRGB

s :: Diagram Cairo
s = fc (sRGB 0.4 0.4 0.4) square

main = renderDia Cairo (CairoOptions "Square.pdf" $ PDF (400, 400)) s
