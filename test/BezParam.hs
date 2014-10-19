{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude

import Diagrams.Backend.Cairo.CmdLine

t :: Trail V2 Double
t = (polygonPath with {sides = 3, orientation = OrientToX})
[v1,v2] = trailOffsets t

seg = Cubic v1 v1 (v1 ^+^ v2)

s :: Trail V2 Double
s = fromSegments [seg]

pts = map ((origin .+^) . (seg `atParam`)) [0,0.01..1]
showPts = position (pts `zip` repeat dot)
  where dot = circle 0.01 # lw 0 # fc red

-- s1 = (strokeT $ s <> (rotateBy (1/3) s) <> (rotateBy (2/3) s))
--    # centerXY

dia :: Diagram Cairo
dia = pad 1.1 (showPts `atop` strokeT s)

main = defaultMain dia
