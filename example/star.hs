import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Data.Colour.Names

d = fc blue $ starPolygon 17 5

main = renderDia Cairo (CairoOptions "Star.pdf" (PDF (100.0,100.0))) d
