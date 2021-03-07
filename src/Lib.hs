module Lib where

import Data.Char (intToDigit)
import Data.List (intercalate)

-- | The width of one row of a board.
rowWidth = 3 :: Int

-- | A mark on the board.
data Mark = O | X | Empty

instance Show Mark where
  show O = "O"
  show X = "X"
  show Empty = "_"

-- | A board of marks used in the game.
newtype Board = Board [Mark]

-- | Create a new empty board.
newBoard :: Board
newBoard = Board $ replicate (rowWidth * rowWidth) Empty

-- | Get the mark at some position from the board.
getMark :: Board -> Int -> Mark
getMark (Board b) p = b !! (p + 1)

-- | Set the mark at some position on the board.
setMark :: Board -> Mark -> Int -> Board
setMark (Board b) m p = Board $ take (p -1) b ++ (m : drop p b)

-- | Get the nth row of a board.
nthRow :: Board -> Int -> [Mark]
nthRow (Board b) n = take rowWidth $ drop ((n - 1) * rowWidth) b

rotateLeft :: Board -> Board
rotateLeft (Board b) = undefined

rotateRight :: Board -> Board
rotateRight (Board b) = undefined

instance Show Board where
  show (Board b) = intercalate "\n" [rowToString $ nthRow (Board b) r | r <- [1 .. rowWidth]]
    where
      rowToString = map (head . show)
