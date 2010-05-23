{-# LANGUAGE PackageImports #-}

import "diagrams-core" Graphics.Rendering.Diagrams
import Graphics.Rendering.Diagrams.Transform

import Diagrams.Combinators
import Diagrams.TwoD
import Diagrams.Path

import Diagrams.Backend.Cairo

d :: Diagram Cairo
d = translate (30,30) $ scale 10 (path [(1,1), (1,2), (1,3), (7,2)])

main = renderDia Cairo [] d