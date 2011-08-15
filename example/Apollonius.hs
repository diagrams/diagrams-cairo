import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Test.QuickCheck

-- Center and signed radius
data Circle = Circle { center :: P2, sRadius :: Double }

descartesR sgn r1 r2 r3 =
  (r1 * r2 * r3) /
    (r1*r2 + r1*r3 + r2*r3 + sgn * 2 * sqrt(r1*r2*r3*(r1+r2+r3)))

prop_descartes (NonZero r1) (NonZero r2) (NonZero r3) = abs (2*(e1*e1 + e2*e2 + e3*e3 + e4*e4) - (e1+e2+e3+e4)^2) < 1e-10
  where [e1,e2,e3,e4] = map (1/) [r1,r2,r3,r4]
        r4 = descartesInR r1 r2 r3

descartesOutR = descartesR (-1)
descartesInR  = descartesR 1

main = defaultMain (circle (descartesInR 1 1 1) <> circle (descartesOutR 1 1 1) <> circles)

circles = decoratePath (polygon with {sides=3} # scale (2/sqrt 3)) (repeat unitCircle)
