import Diagrams.Prelude
import Diagrams.TwoD.Path
import Diagrams.Backend.Cairo.CmdLine

import qualified Data.Set as S

-- t = fromOffsets [(1,5), (1,-5)]
-- path = close . pathFromTrail . mconcat . take 8 . iterate (rotateBy (-1/8)) $ t

       -- polygonPath with {sides=8}

-- path = close . fromOffsets $ [(0,10), (3,0), (0, -8), (2,0), (0,2), (-4,0), (0, 3)
--                              ,(9, 0), (0, -7)]

t = (polygonPath with {sides = 3, orientation = OrientToX})
[v1,v2] = trailOffsets t

seg = Cubic v1 v1 (v1 ^+^ v2)

path = scale 5 . close $ fromSegments [seg]

dot c = circle # fc c
               # lw 0
               # scale 0.1

points = mconcat [ if (isInsideWinding p path) then dot red # moveTo p else mempty
                 | x <- range
                 , y <- range
                 , let p = P (x,y)
                 ]
  where range = [-10, -9.8 .. 10]

main = defaultMain (points <> stroke path # lw 0.05)