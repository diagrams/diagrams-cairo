{-# LANGUAGE NoMonomorphismRestriction #-}
import Diagrams.Prelude hiding (Result)

import Diagrams.Backend.Cairo.CmdLine

import Data.Tree
import Control.Arrow
import Data.Maybe
import Data.Function
import Data.Ord
import Data.List (sortBy, transpose, maximumBy)

type D = Diagram Cairo R2

hrule :: D
hrule = (strokeT $ fromOffsets [(3,0)]) # lw 0.05 # freeze
vrule = hrule # rotateBy (1/4)

grid = horiz `atop` verts
  where horiz = vcat' with {sep = 1} [hrule,hrule] # centerXY
        verts = hcat' with {sep = 1} [vrule,vrule] # centerXY

ex, oh :: D
ex = (stroke $ fromVertices [P (-1,-1), P (1,1)] <> fromVertices [P (1,-1), P (-1,1)])
   # lw 0.05
   # lineCap LineCapRound
   # scale 0.4
   # freeze
oh = circle
   # scale 0.4
   # lw 0.05
   # freeze

data Player = Ex | Oh
  deriving (Show, Eq)
data Piece  = Filled Player | Empty
  deriving (Show, Eq)

type Board = [[Piece]]

turn Ex = Oh
turn Oh = Ex

emptyBoard = replicate 3 (replicate 3 Empty)

renderBoard :: Board -> D
renderBoard b = (renderPieces b # centerXY)
  where renderPieces = vcat' with {catMethod = Distrib}
                     . map (hcat' with {catMethod = Distrib})
                     . map (map renderPiece)

renderPiece :: Piece -> D
renderPiece (Filled Ex) = ex
renderPiece (Filled Oh) = oh
renderPiece Empty       = mempty

-- unfoldTree :: (b -> (a, [b])) -> b -> Tree a
gameTree :: Tree (Player, Board)
gameTree = unfoldTree (id &&& uncurry moves) (Ex, emptyBoard)

moves :: Player -> Board -> [(Player, Board)]
moves play brd = map ((,) (turn play)) (catMaybes . map (flip moveAt brd) $ coords)
  where mkMove Empty = Just $ Filled play
        mkMove _     = Nothing
        moveAt (i,j) = modList i (modList j mkMove)
        coords = [(i,j) | i <- [0..2], j <- [0..2]]

modList :: Int -> (a -> Maybe a) -> [a] -> Maybe [a]
modList i f xs = case splitAt i xs of
                   (pre, (a:post)) -> f a >>= \a' -> Just $ pre ++ (a' : post)
                   _               -> Nothing

data Result = Win Player
            | Cats

-- pruneGameTreeFor :: Player -> Tree (Player, Board) -> Tree (Player, Board)
-- pruneGameTreeFor

prune :: Player -> Tree (Player, Board) -> (Tree (Player, Board), Result)
prune player (Node (whoseMove, board) ch)
  | isWin board  = ((Node (whoseMove, board) []), Win $ turn whoseMove)
  | isFull board = ((Node (whoseMove, board) []), Cats)
  | otherwise    = (Node (whoseMove, board) ch'', maximumBy (comparing toInt) (map snd ch'))
      where ch'  = map (prune player) ch
            ch'' | player == whoseMove
                 = [fst . head . sortBy (compare `on` (toInt . snd)) $ ch']
                 | otherwise = map fst ch'
            toInt Cats    = 0
            toInt (Win p) | player == p = 1
                          | otherwise   = -1

isWin :: Board -> Bool
isWin board = any isRowWin board || any isRowWin (transpose board)
           || isRowWin [board!!0!!0, board!!1!!1, board!!2!!2]
           || isRowWin [board!!0!!2, board!!1!!1, board!!2!!0]
  where isRowWin [x@(Filled _),y,z] = x == y && y == z
        isRowWin _              = False

isFull :: Board -> Bool
isFull board = not (any (any (==Empty)) board)

testB = [[Filled Ex, Filled Oh, Empty],
         [Empty, Filled Ex, Empty],
         [Filled Ex, Filled Oh, Filled Ex]]

main = do --print . take 3 . levels . fst . prune Oh $ gameTree
          print (isWin testB)
          defaultMain mempty