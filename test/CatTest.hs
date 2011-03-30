import Diagrams.Prelude

import Diagrams.Backend.Cairo.CmdLine

import Data.Colour.Names
import Data.List
import Data.Maybe

myColors = cycle $ [ aqua
                   , aquamarine
                   , blue
                   , blueviolet
                   , cadetblue
                   , cyan
                   , cornflowerblue
                   , darkblue
                   , darkcyan
                   ]

circles = map (lw 0) $
  zipWith3 (\c s -> fc c . scale s) myColors [1..10] (repeat circle)

foo, bar :: Diagram Cairo R2
foo = cat (1,-0.5) $ circles
bar = decorateTrail (scale 30 $ polygonPath with { sides = length circles }) circles

-- bar = cat (1,0) [fc blue circle, circle, circle]

main = defaultMain (beside (0,-1) foo bar)