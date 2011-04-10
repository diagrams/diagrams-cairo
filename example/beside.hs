{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
import Diagrams.Prelude

import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.CmdLine

import Diagrams.TwoD.Ellipse

b :: (Backend b R2, Renderable Ellipse b, Renderable (Path R2) b) => Diagram b R2
b = beside (1,1) (rotate (pi/4) $ scaleY 2 circle) square

e1 :: (Backend b R2, Renderable Ellipse b, Renderable (Path R2) b) => Diagram b R2
e1 = rotate (pi/4) $ scaleY 2 circle

e2 :: (Backend b R2, Renderable Ellipse b, Renderable (Path R2) b) => Diagram b R2
e2 = translateX 1 $ e1

-- Move a function like this into the standard library.  It should
-- make the size of the origin be a percentage of the diagram size,
-- configurable color, and continue using the bounds of the original
-- diagram.
showOrigin :: (Backend b R2, Renderable Ellipse b) => Diagram b R2 -> Diagram b R2
showOrigin = ((circle # scale 0.1 # fc red # lw 0) `atop`)

main :: IO ()
main = defaultMain b
