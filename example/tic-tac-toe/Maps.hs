{-# LANGUAGE NoMonomorphismRestriction #-}

-- Maps of optimal tic-tac-toe play, inspired by similar maps created
-- by Randall Munroe, http://xkcd.com/832/

import Diagrams.Prelude hiding (Result)
import Diagrams.Backend.Cairo.CmdLine

import Data.List.Split (chunk)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Map as M
import Data.Tree
import Control.Arrow (second, (&&&))
import Data.Array (assocs)

import Solve

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

getMove (Node (Game _ _ (Move _ m : _), _) _) = m

renderSolved :: Tree (Game, Result) -> D
renderSolved (Node (Game board player1 moves, _)
                [Node (Game board' player2 (Move _ loc : moves'), _) conts])
    = renderGrid g
  where g = M.fromList $
              [(loc, renderPlayer player1 # lc red)] ++
              ((map . second) renderPlayer . catMaybes . map strength . assocs $ board) ++
              (map (getMove &&& (scale (1/3) . renderSolved)) $ conts)

renderSolved _ = error "renderSolved should be called on solved trees only"

strength :: Functor f => (a, f b) -> f (a,b)
strength (a, f) = fmap ((,) a) f

renderPlayer X = x
renderPlayer O = o

renderGrid :: M.Map Loc D -> D
renderGrid g
  = grid
  . chunk 3
  . map (fromMaybe (phantom x) . flip M.lookup g)
  $ [ (r,c) | r <- [0..2], c <- [0..2] ]

treeTake :: Int -> Tree a -> Tree a
treeTake 0 (Node a _)  = Node a []
treeTake n (Node a ts) = Node a (map (treeTake (n-1)) ts)

main = defaultMain (pad 1.1 . renderSolved . solveFor X $ gameTree)
