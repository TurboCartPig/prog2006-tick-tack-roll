module Game (gameloop, GameState (GameState), PlayerType (Cpu, Human)) where

import           Board
import           Common
import           System.Random (randomRIO)
import           Text.Read     (readMaybe)

-- | Parse the input string from the player into game actions.
-- | NOTE: DocTests are a bit broken due to overrideing how types are Read and Shown.
--
-- >>> parsePlayerCommand "5"
-- Just (5,Nothing)
--
-- >>> parsePlayerCommand "5 right"
-- Just (5,Just right)
--
-- >>> parsePlayerCommand "1 left"
-- Just (1,Just left)
--
-- >>> parsePlayerCommand "this is not valid"
-- Nothing
parsePlayerCommand :: String -> Maybe (Int, Maybe Rotation)
parsePlayerCommand s =
  let cmd = words s
      rotation = tailM cmd >>= headM >>= readMaybe
      index = headM cmd >>= readMaybe
   in (\i -> (i, rotation)) <$> index

-- | PlayerType is the type of player controlling one mark.
data PlayerType = Cpu | Human
  deriving (Eq)

-- | GameState is the state of the entire game.
data GameState = GameState
  { -- | Currently active player.
    current :: Mark,
    -- | What kind of player is the player of mark X.
    playerX :: PlayerType,
    -- | Same as for playerX, but for mark O.
    playerO :: PlayerType,
    -- | The game board.
    board   :: Board
  }

-- | Main gameloop.
gameloop :: GameState -> IO ()
gameloop (GameState current px po board) = do
  let next = if current == X then O else X
  let player = if current == X then px else po

  board' <- makeMove' board current player

  -- Check if player has won
  case board' of
    Just board'' -> do
      case checkIfWon board'' current of
        True  -> putStrLn $ "GAME OVER! " <> show current <> " Won!"
        False -> do
          case checkIfDraw board'' of
            True  -> putStrLn "GAME OVER! DRAW!"
            False -> gameloop $ GameState next px po board''
    Nothing -> putStrLn "GAME OVER! Illegal move"

-- | Make a move on the board based on whether the current player is a human or cpu.
makeMove' :: Board -> Mark -> PlayerType -> IO (Maybe Board)
makeMove' board mark Cpu   = return <$> makeMoveCpu board mark
makeMove' board mark Human = makeMoveHuman board mark

-- | Make a move on the board by prompting the user for input and doing what they say.
makeMoveHuman :: Board -> Mark -> IO (Maybe Board)
makeMoveHuman board mark = do
  -- Print the board so that the human player always sees where they are placing their mark
  print board

  -- Get player input command
  input <- getLine
  return $ makeMoveHuman' board mark input

makeMoveHuman' :: Board -> Mark -> String -> Maybe Board
makeMoveHuman' board mark input = do
  (index, rotation) <- parsePlayerCommand input
  board' <- makeMove board mark index
  return $ case rotation of
    Just r  -> rotateBoard board' r
    Nothing -> board'

-- | Make a move on the board by randomly choosing one of the currently possible moves.
makeMoveCpu :: Board -> Mark -> IO Board
makeMoveCpu board mark = do
  let moves = getPossibleMoves board
  -- Pick a random move out of all the remaining possible moves
  move <- (!!) moves <$> randomRIO (0, length moves - 1)
  return $ setMark board mark move
