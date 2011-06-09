import Diagrams.Prelude
import Diagrams.TwoD.Path
import Diagrams.Backend.Cairo.CmdLine

import qualified Data.Set as S

path = polygonPath with {sides = 8}

-- t = fromOffsets [(1,5), (1,-5)]
-- path = close . pathFromTrail . mconcat . take 8 . iterate (rotateBy (-1/8)) $ t

-- path = close . fromOffsets $ [(0,10), (3,0), (0, -8), (2,0), (0,2), (-4,0), (0, 3)
--                              ,(9, 0), (0, -7)]

-- t = (polygonPath with {sides = 3, orientation = OrientToX})
-- [v1,v2] = trailOffsets t

-- seg = Cubic v1 v1 (v1 ^+^ v2)

-- path = centerXY . scale 5 . close $ fromSegments [seg]

dot c = circle 0.01
        # fc c
        # lw 0

points = mconcat [ if getAny (sample oct p) then dot red # moveTo p else mempty
                 | x <- range
                 , y <- range
                 , let p = P (x,y)
                 ]
  where range = [-1, -0.96 .. 1]

oct = stroke path # lw 0.02

main = defaultMain (points <> oct)