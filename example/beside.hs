{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
import Diagrams.Prelude

import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.CmdLine

import Diagrams.TwoD.Ellipse

b :: (BSpace b ~ R2, Renderable Ellipse b, Renderable (Path R2) b) => Diagram b
b = beside (1,1) (rotate (pi/4) $ scaleY 2 circle) square

e1 :: (BSpace b ~ R2, Renderable Ellipse b, Renderable (Path R2) b) => Diagram b
e1 = rotate (pi/4) $ scaleY 2 circle

e2 :: (BSpace b ~ R2, Renderable Ellipse b, Renderable (Path R2) b) => Diagram b
e2 = translateX 1 $ e1

main :: IO ()
main = defaultMain b
