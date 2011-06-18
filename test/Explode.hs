{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

u = vrule 1 # reflectY
r = hrule 1

tr = mconcat [u, u, r, u, r, r, r, u]

ps = explodeTrail origin tr

d = strokeT tr
    # lc red
    # centerXY

d2 = ps
     # map assignColor
     # map (\(p,c) -> lc c . stroke $ p)
     # lw 0.05
     # centerXY
     # mconcat

assignColor p | direction v < 1/8 = (p,red)
              | otherwise         = (p,blue)
  where [v] = pathOffsets p

main = defaultMain (pad 1.1 d2)