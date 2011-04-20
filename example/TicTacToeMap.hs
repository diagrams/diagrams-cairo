{-# LANGUAGE NoMonomorphismRestriction #-}

-- Maps of optimal tic-tac-toe play, inspired by similar maps created
-- by Randall Munroe, http://xkcd.com/832/

import Diagrams.Prelude hiding (Result)
import Diagrams.Backend.Cairo.CmdLine

import SolveTicTacToe

type D = Diagram Cairo R2

x, o :: D
x = (stroke $ fromVertices [P (-1,1), P (1,-1)] <> fromVertices [P (1,1), P (-1,-1)])
  # lw 0.05
  # lineCap LineCapRound
  # scale 0.4
  # freeze
  # centerXY
o = circle
  # lw 0.05
  # scale 0.4
  # freeze

grid :: [[D]] -> D
grid = centerXY
     . vcat' with {catMethod = Distrib}
     . map (hcat' with {catMethod = Distrib})

renderMapFor :: Player -> Tree (Game, Result) -> D
renderMapFor p (Node (Game board player moves) conts)

treeTake :: Int -> Tree a -> Tree a
treeTake 0 (Node a _)  = Node a []
treeTake n (Node a ts) = Node a (map (treeTake (n-1)) ts)

main = defaultMain (pad 1.1 $ grid [ [x,x,o]
                         , [o,mempty,x]
                         , [mempty, mempty, x]])