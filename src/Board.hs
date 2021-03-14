module Board (Rotation, Board, Mark (X, O, Empty), checkIfWon, setMark, getMark, makeMove, getPossibleMoves, newBoard, rotateBoard) where

import Common
import Data.Char (intToDigit)
import Data.List (intercalate)

-- | The width of one row of a board.
rowWidth = 3 :: Int

-- | Board rotation direction.
data Rotation = RLeft | RRight
  deriving (Show, Read)

-- | A mark on the board.
data Mark = O | X | Empty
  deriving (Eq)

instance Show Mark where
  show O = "O"
  show X = "X"
  show Empty = "_"

-- | A board of marks used in the game.
newtype Board = Board [Mark]

instance Show Board where
  show (Board b) = intercalate "\n" [rowToString $ nthRow (Board b) r | r <- [1 .. rowWidth]]
    where
      rowToString = map (head . show)

-- | Create a new empty board.
newBoard :: Board
newBoard = Board $ replicate (rowWidth * rowWidth) Empty

-- | Get the mark at some position from the board.
--
-- >>> getMark (setMark newBoard X 5) 5
-- X
getMark :: Board -> Int -> Mark
getMark (Board b) p = b !! (p - 1)

-- | Set the mark at some position on the board.
setMark :: Board -> Mark -> Int -> Board
setMark (Board b) m p = Board $ take (p - 1) b ++ (m : drop p b)

-- | Place a mark at a position only if it was not previously taken.
makeMove :: Board -> Mark -> Int -> Maybe Board
makeMove b m i =
  let prev = getMark b i
   in if prev /= Empty
        then Nothing
        else Just $ setMark b m i

-- | Get the nth row of a board.
nthRow :: Board -> Int -> [Mark]
nthRow (Board b) n = take rowWidth $ drop ((n - 1) * rowWidth) b

-- | Get the nth column of a board.
--
-- >>> nthColumn (Board [O, O, X, O, O, X, O, O, X]) 3
-- [X,X,X]
nthColumn :: Board -> Int -> [Mark]
nthColumn (Board b) n = map snd $ filter inRow $ enumerate b
  where
    inRow (i, _) = i `mod` rowWidth == (n - 1)

-- | Check if a mark has won based on board state.
--
-- >>> checkIfWon (Board [X, X, X, Empty, Empty, Empty, Empty, Empty, Empty]) X
-- True
--
-- >>> checkIfWon (Board [X, Empty, Empty, X, Empty, Empty, X, Empty, Empty]) X
-- True
--
-- >>> checkIfWon (Board [X, Empty, Empty, Empty, X, Empty, Empty, Empty, X]) X
-- True
--
-- >>> checkIfWon (Board [Empty, Empty, X, Empty, X, Empty, X, Empty, Empty]) X
-- True
checkIfWon :: Board -> Mark -> Bool
checkIfWon (Board b) mark =
  let rows = or [all (== mark) (nthRow (Board b) r) | r <- [1 .. rowWidth]] :: Bool
      columns = or [all (== mark) (nthColumn (Board b) c) | c <- [1 .. rowWidth]] :: Bool
      diagonal1 = all (== mark) [b !! d | d <- take rowWidth [0, rowWidth + 1 ..]] :: Bool
      diagonal2 = all (== mark) [b !! d | d <- take rowWidth [rowWidth - 1, rowWidth * 2 - 2 ..]] :: Bool
   in rows || columns || diagonal1 || diagonal2

-- | Get all the possible moves (indecies of empty places) left to make on a board.
getPossibleMoves :: Board -> [Int]
getPossibleMoves (Board b) = map ((+ 1) . fst) $ filter (\(_, x) -> x == Empty) $ enumerate b

-- | First swap the corners and then rotate the board.
rotateBoard :: Board -> Rotation -> Board
rotateBoard b r = case r of
  RRight -> rotateRight . swapCorners $ b
  RLeft -> rotateLeft . swapCorners $ b

-- From here on functions require the board to be 3x3
-- ---------------------------------------------------------------------------------------------

swapCorners :: Board -> Board
swapCorners (Board (a : b : c : xs)) = Board $ c : b : a : xs
swapCorners _ = undefined -- Other lenghts are not handled yet

rotateLeft :: Board -> Board
rotateLeft (Board [a, b, c, d, e, f, g, h, i]) = Board [c, f, i, b, e, h, a, d, g]
rotateLeft _ = undefined -- Other lenghts are not handled yet

rotateRight :: Board -> Board
rotateRight (Board [a, b, c, d, e, f, g, h, i]) = Board [g, d, a, h, e, b, i, f, c]
rotateRight _ = undefined -- Other lenghts are not handled yet
