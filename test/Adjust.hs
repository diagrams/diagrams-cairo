{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

-- s = Linear (1,1)
-- s' = adjustSegment s with { adjMethod = ByParam 0.3, adjSide = Both }

s = Cubic (-1.469563859939651,-7.0543643759197145) (-15.71112972096714,10.354074559684419) (-3.041713868997588,-5.064951590019016)
-- arc length = 18.5

s' = adjustSegment s with { adjMethod = ToAbsolute 5.0, adjSide = End }

s2 = Cubic (0.8481056976928445,-0.7672709057378255) (1.1571739382091348,0.3803092030511929) (1.2613155540667786,-0.6362037010168534)

s2' = adjustSegment s2 with { adjMethod = ToAbsolute 0.8329103042366314, adjSide = Both }

p1 = fromSegments [s2'] # lc red
p2 = fromSegments [s2]

d = p1 <> p2 <> square 1 <> b # centerXY

b = square 2 # lw 0

main = defaultMain (pad 1.1 $ d)