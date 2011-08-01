{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

-- s = Linear (1,1)
-- s' = adjustSegment s with { adjMethod = ByParam 0.3, adjSide = Both }

s = Cubic (-1.469563859939651,-7.0543643759197145) (-15.71112972096714,10.354074559684419) (-3.041713868997588,-5.064951590019016)
-- arc length = 18.5

s' = adjustSegment s with { adjMethod = ToAbsolute 5.0, adjSide = Both }

p = fromSegments [s']

d = p <> square 1 <> b # centerXY

b = square 2 # lw 0

main = defaultMain (pad 1.1 $ d)