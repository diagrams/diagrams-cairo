{-# LANGUAGE PackageImports #-}

import "diagrams-core" Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Transform

import Diagrams.TwoD

import Diagrams.Backend.Cairo
import Diagrams.Combinators

d :: Diagram Cairo
d = translate (30,30) $ scale 10 (beside (2,1) box box `atop` circle)

d' :: Diagram Cairo
d' = translate (30,30) $ scale 10 (beside (2,1) box box `atop` ell1)

ell1 = rotate (-pi/6) $ scaleX 2 $ scaleY 0.5 circle

d2, d2', d3, d4, d5 :: Diagram Cairo
d2 = translate (30,30) $ scale 10 (beside (2,1) box ell1)  -- intersects
d2' = translate (30,30) $ scale 10 (beside (4,1) box ell1)  -- intersects

d3 = translate (30,30) $ scale 10 (beside (2,1) box circle)
d4 = translate (30,30) $ scale 10 (beside (50,50) box ell1)
d5 = translate (30,30) $ scale 10 (beside (2,2) box circle)

e :: Diagram Cairo
e = translate (50,70) $ rotate (pi/6) $ scaleX 30 $ scaleY 10 circle

ell = rotate (pi/6) $ scaleX 30 $ scaleY 10 circle
box10 = scale 10 box

f :: Diagram Cairo
f = translate (50,70) $ beside (1,0) box10 ell

opts = CairoOptions "test3.pdf" $ PDF (400, 400)
main = renderDia Cairo opts d2'
