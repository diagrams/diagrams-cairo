module SolveTicTacToe where

import Data.Array
import Data.Tree
import Data.Function (on)
import Data.List (groupBy, maximumBy)
import Data.Maybe (isNothing, isJust)
import Control.Applicative (liftA2)
import Control.Arrow ((&&&))

data Player = X | O
  deriving (Show, Eq, Ord)

next X = O
next O = X

data Result = Win Player
            | Cats
  deriving (Show, Eq)

compareResultsFor :: Player -> (Result -> Result -> Ordering)
compareResultsFor X = compare `on` resultToInt
    where resultToInt (Win X) = 1
          resultToInt Cats    = 0
          resultToInt (Win O) = -1
compareResultsFor O = flip (compareResultsFor X)

type Loc = (Int,Int)
type Board = Array Loc (Maybe Player)

emptyBoard :: Board
emptyBoard = listArray ((0,0), (2,2)) (repeat Nothing)

showBoard :: Board -> String
showBoard = unlines . map showRow . groupBy ((==) `on` (fst . fst)) . assocs
  where showRow = concatMap (showPiece . snd)
        showPiece Nothing  = " "
        showPiece (Just p) = show p

data Move = Move Player Loc
  deriving Show

makeMove :: Move -> Board -> Board
makeMove (Move p l) b = b // [(l, Just p)]

data Game = Game Board           -- ^ The current board state.
                 Player          -- ^ Whose turn is it?
                 [Move]          -- ^ The list of moves so far (most
                                 --   recent first).
  deriving Show

initialGame = Game emptyBoard X []

-- | The full game tree for tic-tac-toe.
gameTree :: Tree Game
gameTree = unfoldTree (id &&& genMoves) initialGame

-- | Generate all possible successor games from the given game.
genMoves :: Game -> [Game]
genMoves (Game board player moves) = newGames
  where validLocs = map fst . filter (isNothing . snd) . assocs $ board
        newGames  = [Game (makeMove m board) (next player) (m:moves)
                      | p <- validLocs
                      , let m = Move player p
                    ]

-- | Simple fold for Trees.  The Data.Tree module does not provide
--   this.
foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f (Node a ts) = f a (map (foldTree f) ts)

-- | Solve the game for player @p@: prune all but the optimal moves
--   for player @p@, and annotate each game with its result (given
--   best play).
solveFor :: Player -> Tree Game -> Tree (Game, Result)
solveFor p = foldTree (solveStep p)

-- | Given a game and its continuations (including their results),
--   solve the game for player p.  If it is player p's turn, prune all
--   continuations except the optimal one for p. Otherwise, leave all
--   continuations.  The result of this game is the result of the
--   optimal choice if it is p's turn, otherwise the worst possible
--   outcome for p.
solveStep :: Player -> Game -> [Tree (Game, Result)] -> Tree (Game, Result)
solveStep p g@(Game brd curPlayer moves) conts
  | isWin g        = Node (g, Win $ next curPlayer) []
  | isFull g       = Node (g, Cats) []
  | curPlayer == p = let c   = bestContFor p conts
                         res = snd . rootLabel $ c
                     in  Node (g, res) [c]
  | otherwise      = Node (g, bestResultFor (next p) conts) conts

bestContFor :: Player -> [Tree (Game, Result)] -> Tree (Game, Result)
bestContFor p = maximumBy (compareResultsFor p `on` (snd . rootLabel))

bestResultFor :: Player -> [Tree (Game, Result)] -> Result
bestResultFor p = snd . rootLabel . bestContFor p

-- | Check whether a game is a win for some player.
isWin :: Game -> Bool
isWin (Game board _ _) = any isLine (rows ++ cols ++ diags)
  where isLine = liftA2 (||) (all (== Just X)) (all (== Just O)) . map (board!)
        rows   = [ [ (r,c) | c <- [0..2] ] | r <- [0..2] ]
        cols   = [ [ (r,c) | r <- [0..2] ] | c <- [0..2] ]
        diags  = [ [ (i,i) | i <- [0..2] ]
                 , [ (i,2-i) | i <- [0..2] ]
                 ]

-- | Check whether a game is full, i.e. there are no blank squares. If
--   'isWin' returns false and 'isFull' is true, it is a cats game.
isFull :: Game -> Bool
isFull (Game board _ _) = all isJust (elems board)
