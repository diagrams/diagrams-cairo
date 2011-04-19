{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude

import Diagrams.Backend.Cairo.CmdLine

type D = Diagram Cairo R2

p, bez, ell :: D
p = stroke $ fromSegments [Linear (1.0,0.0),Linear (0.0,1.0)]
bez = stroke $ fromSegments [Cubic (1.0,0.0) (0.0,1.0) (1.0,1.0)]
ell = scaleX 2 $ scaleY 0.5 circle

t = (polygonPath with {sides = 3, orientation = OrientToX})
[v1,v2] = trailOffsets t

s :: Trail R2
s = fromSegments [Cubic v1 v1 (v1 ^+^ v2)]


s1 = (strokeT $ s)
   # centerXY

s2 = (strokeT $ s <> (rotateBy (1/3) s))
   # centerXY

s3 = (strokeT $ s <> (rotateBy (1/3) s) <> (rotateBy (2/3) s))
   # centerXY

b1 = runBoundsTest square
b2 = runBoundsTest circle
b3 = runBoundsTest p
b4 = runBoundsTest bez
b5 = runBoundsTest ell
b6 = runBoundsTest (scale 2 square)
b7 = runBoundsTest (scale 2 circle)
b8 = runBoundsTest (scale 2 p)
b9 = runBoundsTest (scale 2 bez)
b10 = runBoundsTest (scale 2 ell)
b11 = runBoundsTest (rotate (pi/6) square)
b12 = runBoundsTest (scaleX 3 $ scaleY 2 $ bez)
b13 = runBoundsTest (translate (1,0) square)
b14 = runBoundsTest (rotate (2*pi/3) ell)
b15 = runBoundsTest s1
b16 = runBoundsTest s2
b17 = runBoundsTest s3

runBoundsTest :: D -> D
runBoundsTest = lw 0.05 . sampleBounds2D 100

sampleBounds2D :: Int -> D -> D
sampleBounds2D n d = foldr atop d bs
    where b  = getBounds (bounds d)
          bs :: [D]
          bs = [stroke $ mkLine (P $ s *^ v) (perp v) | v <- vs, let s = b v]
          vs = [(2 * cos t, 2 * sin t) | i <- [0..n]
                                       , let t = ((fromIntegral i) * 2.0 * pi) / (fromIntegral n)]
          mkLine a v = setStart a $ fromSegments [Linear v]
          perp (x,y) = (-y,x)
          getBounds (Bounds f) = f

main = defaultMain b15
