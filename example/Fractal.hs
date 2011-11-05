import Diagrams.Prelude

import qualified Data.Colour as C
import Diagrams.Backend.Cairo.CmdLine

type DC = Diagram Cairo R2

sierpinski :: Int -> DC
sierpinski 1 = t
sierpinski n =     s
                  ===
               (s ||| s)
  where s = sierpinski (n-1)

t = regPoly 3 1
    # lw 0
    # fc black

colors = iterate (C.blend 0.1 white) blue

pentaflake :: Int -> DC
pentaflake 0 = p
pentaflake n = appends (p' # fc (colors !! (n-1)))
                       (zip vs (repeat (rotateBy (1/2) p')))
  where vs = take 5 . iterate (rotateBy (1/5))
                    . (if odd n then negateV else id) $ unitY
        p' = pentaflake (n-1)

pentaflake' n = pentaflake n # fc (colors !! n)

p = regPoly 5 1 # lw 0

main = defaultMain (pad 1.1 $ pentaflake' 4)