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
  zipWith3 (\c s d -> d # fc c # scale s) myColors [1..10] (repeat circle)

foo1 = cat (1,-0.5) $ circles
foo2 = cat unitX $ map alignTop circles
foo3 = cat unitX $ map alignBottom circles
bar = decorateTrail (polygonPath with { sides = length circles } # scale 30)
                    circles

showOrigin = ((circle # fc red # lw 0) `atop`)

dia = cat (negateV unitY) (map (showOrigin . centerX) [foo1, foo2, foo3, bar])

main = defaultMain dia