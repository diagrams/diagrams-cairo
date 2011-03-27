import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Shapes
import Data.Colour.SRGB

sierpinski :: Int -> Diagram Cairo
sierpinski 0 = fc (sRGB 0.4 0.4 0.4) $ lw 0 $ (rotate (pi/2) (polygon with {sides=3}))
sierpinski n = beside (0, 1) (beside (1, 0) s s) s
  where s = sierpinski (n-1)

s :: Diagram Cairo
s = sierpinski 6

main = defaultMain s
