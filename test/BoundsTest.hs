{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude

import Diagrams.Backend.Cairo.CmdLine

type D = Diagram Cairo

p, bez, ell :: D
p = stroke $ fromSegments [Linear (1.0,0.0),Linear (0.0,1.0)]
bez = stroke $ fromSegments [Cubic (1.0,0.0) (0.0,1.0) (1.0,1.0)]
ell = scaleX 2 $ scaleY 0.5 unitCircle

t = (polygonPath with {sides = 3, orientation = OrientToX})
[v1,v2] = trailOffsets t

s :: Trail V2 Double
s = fromSegments [Cubic v1 v1 (v1 ^+^ v2)]

s1 = (strokeT $ s)
   # centerXY

s2 = (strokeT $ s <> (rotateBy (1/3) s))
   # centerXY

s3 = (strokeT $ s <> (rotateBy (1/3) s) <> (rotateBy (2/3) s))
   # centerXY

str :: D
str = strokeT $ fromOffsets [(1,0)]

b1 = runBoundsTest $ square 1
b2 = runBoundsTest $ circle 1
b3 = runBoundsTest p
b4 = runBoundsTest bez
b5 = runBoundsTest ell
b6 = runBoundsTest $ square 2
b7 = runBoundsTest $ circle 2
b8 = runBoundsTest (scale 2 p)
b9 = runBoundsTest (scale 2 bez)
b10 = runBoundsTest (scale 2 ell)
b11 = runBoundsTest (rotate (pi/6 :: Rad) unitSquare)
b12 = runBoundsTest (scaleX 3 $ scaleY 2 $ bez)
b13 = runBoundsTest (translate (1,0) unitSquare)
b14 = runBoundsTest (rotate (2*pi/3 :: Rad) ell)
b15 = runBoundsTest s1
b16 = runBoundsTest s2
b17 = runBoundsTest s3
b18 = runBoundsTest str

runBoundsTest :: D -> D
runBoundsTest = lw 0.05 . sampleBounds2D 100

sampleBounds2D :: Int -> D -> D
sampleBounds2D n d = foldr (flip atop) (d # lc red) bs
    where b  = getBounds (bounds d)
          bs :: [D]
          bs = [stroke $ mkLine (P $ s *^ v) (perp v) | v <- vs, let s = b v]
          vs = [(2 * cos t, 2 * sin t) | i <- [0..n]
                                       , let t = ((fromIntegral i) * 2.0 * pi) / (fromIntegral n)]
          mkLine a v = setStart a $ fromSegments [Linear v]
          perp (x,y) = (-y,x)
          getBounds (Bounds f) = f

main = defaultMain b18
