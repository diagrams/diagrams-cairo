import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.TwoD.Shapes
import Data.Colour.SRGB

sierpinski :: Int -> Diagram Cairo
sierpinski 0 = fc (sRGB 0.4 0.4 0.4) $ lw 0 $ (rotate (pi/2) (polygon 3))
sierpinski n = beside (0, 1) (beside (1, 0) s s) s
  where s = sierpinski (n-1)

s :: Diagram Cairo
s = sierpinski 6

main = renderDia Cairo (CairoOptions "Sierpinski.pdf" (PDF (100.0,100.0))) s
