{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude

import Diagrams.Backend.Cairo.CmdLine

type D = Diagram Cairo

t = (polygonPath with {sides = 3, orientation = OrientToX})
[v1,v2] = trailOffsets t

s :: Trail V2 Double
s = fromSegments [Cubic v1 v1 (v1 ^+^ v2)]

s1 = (strokeT $ s <> (rotateBy (1/3) s) <> (rotateBy (2/3) s))
   # centerXY

dia :: D
dia = pad 1.1 (s1 ||| unitSquare)

main = defaultMain dia
