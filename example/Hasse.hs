{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Data.List
import Data.Ord (comparing)
import Data.Function (on)

type D = Diagram Cairo R2

colors = [black, blue, red, yellow, green, orange, purple, brown]

data Subset = Subset Int [Int]

(Subset _ elts1) `isSubset` (Subset _ elts2) = all (`elem` elts2) elts1

subsetsBySize :: Int -> [[Subset]]
subsetsBySize n = map (map (Subset n))
                . groupBy ((==) `on` length)
                . sortBy (comparing length)
                . subsequences
                $ [1..n]

drawElts n elts = hcat . map (\i -> if i `elem` elts then drawElt i else strutX 1) $ [1..n]
drawElt e = unitSquare # fc (colors !! e) # lw 0.05 # freeze

drawSet :: Subset -> D
drawSet (Subset n elts) = (    drawElts n elts # centerXY
                            <> rect (fromIntegral n + 0.5) 1.5
                                 # dashing [0.2,0.2] 0
                                 # lw 0.03
                                 # named elts
                          )
                          # freeze

hasseRow = centerX . hcat' with {sep = 2} . map drawSet

hasseDiagram n = setsD # drawConnections
  where setsD = vcat' with {sep = fromIntegral n} . map hasseRow . reverse $ subsets
        drawConnections = applyAll connections
        connections = concat $ zipWith connectSome subsets (tail subsets)
        connectSome subs1 subs2 = [ connect s1 s2 | s1 <- subs1
                                                  , s2 <- subs2
                                                  , s1 `isSubset` s2 ]
        connect (Subset _ elts1) (Subset _ elts2) =
          withANameB elts1 $ \p1 b1 ->
          withANameB elts2 $ \p2 b2 ->
          (<> stroke (fromVertices [ boundaryFrom p1 unitY b1
                                   , boundaryFrom p2 unit_Y b2
                                   ]) # lw 0.03)
        subsets = subsetsBySize n

main = defaultMain (pad 1.1 $ hasseDiagram 4)