{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Data.Monoid

type D = Diagram Cairo

dot = circle 0.1
      # lw 0 # fc black
      # named "dot"

d = decorateTrail (polygon with {sides = 8}) (repeat dot)
    # mkConnections
  where indices = [0,2,5,6,3,7] :: [Int]
        mkConnections = applyAll $
                        zipWith (\i j -> connect (i ||> "dot") (j ||> "dot"))
                          indices (tail indices)

connect :: Name -> Name -> D -> D
connect n1 n2 = withName n1 $ \p1 ->
                  withName n2 $ \p2 ->
                    (<> (p1 ~~ p2))

main = defaultMain (pad 1.1 (d # centerXY))
