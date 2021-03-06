module Board (Rotation (RLeft, RRight), Board (Board), Mark (X, O, Empty), checkIfWon, checkIfDraw, setMark, getMark, makeMove, getPossibleMoves, newBoard, rotateBoard, swapCorners, rotateLeft, rotateRight) where

import           Common    (enumerate)
import           Data.Char (intToDigit)
import           Data.List (intercalate)
import           GHC.Read  (choose, parens, readList, readListDefault,
                            readListPrec, readListPrecDefault, readPrec)

-- | The width of one row of a board.
rowWidth = 3 :: Int

-- | Board rotation direction.
data Rotation = RLeft | RRight

-- | Manually implement Show to control how the type is converted to a string.
instance Show Rotation where
  show RRight = "right"
  show RLeft  = "left"

-- | Manually implement Read to control how the type is constructed from a string.
-- | I stole the implementation ghc generates from deriving(Read),
-- | and modified it to use "left" and "right" as opposed to "RLeft" and "RRight" as it usually would.
-- | You can replicate this using the -ddump-deriv option with ghc (Or --ghc-options="-ddump-deriv" using stack).
-- | This is admittedly overkill and could have been simpler, but ehh.
instance Read Rotation where
  readPrec = parens $ choose [("left", return RLeft), ("right", return RRight)]
  readList = readListDefault
  readListPrec = readListPrecDefault

-- | A mark on the board.
data Mark = O | X | Empty
  deriving (Eq)

instance Show Mark where
  show O     = "O"
  show X     = "X"
  show Empty = "_"

-- | A board of marks used in the game.
newtype Board = Board [Mark]
  deriving (Eq)

instance Show Board where
  show (Board b) = intercalate "\n" ["# " ++ rowToString (nthRow (Board b) r) | r <- [1 .. rowWidth]]
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
--
-- >>> getMark (setMark newBoard X 5) 5
-- X
setMark :: Board -> Mark -> Int -> Board
setMark (Board b) m p = Board $ take (p - 1) b ++ (m : drop p b)

-- | Place a mark at a position only if it was not previously taken.
--
-- >>> makeMove (setMark newBoard X 5) X 5
-- Nothing
makeMove :: Board -> Mark -> Int -> Maybe Board
makeMove b m i =
  let prev = getMark b i
   in if prev /= Empty
        then Nothing
        else Just $ setMark b m i

-- | Get the nth row of a board.
--
-- >>> nthRow (Board [X, X, X, O, O, O, O, O, O]) 1
-- [X,X,X]
nthRow :: Board -> Int -> [Mark]
nthRow _ 0 = error "You probably meant to call this with 1 for first row"
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

-- | Check if there are no more moves to make, aka the match is a draw.
--
-- >>> checkIfDraw newBoard
-- False
--
-- Does not check if some mark has won, only if there are more moves to make.
-- >>> checkIfDraw $ Board $ replicate (rowWidth * rowWidth) X
-- True
checkIfDraw :: Board -> Bool
checkIfDraw (Board b) = null $ filter (== Empty) b

-- | Get all the possible moves (indices of empty places) left to make on a board.
getPossibleMoves :: Board -> [Int]
getPossibleMoves (Board b) = map ((+ 1) . fst) $ filter (\(_, x) -> x == Empty) $ enumerate b

-- | First swap the corners and then rotate the board.
rotateBoard :: Board -> Rotation -> Board
rotateBoard b r = (case r of
    RRight -> rotateRight
    RLeft  -> rotateLeft
  ) . swapCorners $ b

-- From here on functions require the board to be 3x3.
-- See test/Spec.hs for tests of these functions.
-- ---------------------------------------------------------------------------------------------

-- | Swap the corners of the board.
swapCorners :: Board -> Board
swapCorners (Board (a : b : c : xs)) = Board $ c : b : a : xs
swapCorners _                        = error "Other lengths are not handled yet"

-- | Rotate the board left.
rotateLeft :: Board -> Board
rotateLeft (Board [a, b, c, d, e, f, g, h, i]) = Board [c, f, i, b, e, h, a, d, g]
rotateLeft _ = error "Other lengths are not handled yet"

-- | Rotate the board right.
rotateRight :: Board -> Board
rotateRight (Board [a, b, c, d, e, f, g, h, i]) = Board [g, d, a, h, e, b, i, f, c]
rotateRight _ = error "Other lengths are not handled yet"
