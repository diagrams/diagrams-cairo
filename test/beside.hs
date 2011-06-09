{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
import Diagrams.Prelude

import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.CmdLine

import Diagrams.TwoD.Ellipse

b :: (Backend b R2, Renderable Ellipse b, Renderable (Path R2) b) => Diagram b R2
b = beside (1,1) (rotate (pi/4 :: Rad) $ scaleY 2 unitCircle) unitSquare

e1 :: (Backend b R2, Renderable Ellipse b, Renderable (Path R2) b) => Diagram b R2
e1 = rotate (pi/4 :: Rad) $ scaleY 2 unitCircle

e2 :: (Backend b R2, Renderable Ellipse b, Renderable (Path R2) b) => Diagram b R2
e2 = translateX 1 $ e1

main :: IO ()
main = defaultMain b
