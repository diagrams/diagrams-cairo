import Diagrams.Prelude

import qualified Data.Colour as C
import Diagrams.Backend.Cairo.CmdLine

type D = Diagram Cairo R2

sierpinski :: Int -> D
sierpinski 1 = t
sierpinski n =     s
                  ===
               (s ||| s)
  where s = sierpinski (n-1)

t = polygon with { sides = 3, orientation = OrientToX }
    # lw 0
    # fc black

colors = iterate (C.blend 0.1 white) blue

pentaflake :: Int -> D
pentaflake 0 = p
pentaflake n = appends (p' # fc (colors !! (n-1)))
                       (zip vs (repeat (rotateBy (1/2) p')))
  where vs = take 5 . iterate (rotateBy (1/5))
                    . (if odd n then negateV else id) $ unitY
        p' = pentaflake (n-1)

pentaflake' n = pentaflake n # fc (colors !! n)

p = polygon with { sides = 5, orientation = OrientToX }
    # lw 0

showOrigin d = (circle # fc red # scale (0.05)) `atop` d

pad d = withBounds (scale 1.1 d) d

main = defaultMain (pad $ pentaflake' 4)