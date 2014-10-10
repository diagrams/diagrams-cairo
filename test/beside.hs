{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
import Diagrams.Prelude

import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.CmdLine

import Diagrams.TwoD.Ellipse

b :: (Backend b V2 Double, Renderable Ellipse b, Renderable (Path V2 Double) b) => Diagram b V2 Double
b = beside (1,1) (rotate (pi/4 :: Rad) $ scaleY 2 unitCircle) unitSquare

e1 :: (Backend b V2 Double, Renderable Ellipse b, Renderable (Path V2 Double) b) => Diagram b V2 Double
e1 = rotate (pi/4 :: Rad) $ scaleY 2 unitCircle

e2 :: (Backend b V2 Double, Renderable Ellipse b, Renderable (Path V2 Double) b) => Diagram b V2 Double
e2 = translateX 1 $ e1

main :: IO ()
main = defaultMain b
