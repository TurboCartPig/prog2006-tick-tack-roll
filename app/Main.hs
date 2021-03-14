module Main where

import Common
import Control.Monad
import Lib
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import System.Random (randomRIO)
import Text.Read (readMaybe)

gameOverStr = "GAME OVER!" :: String

gameWonBy :: Mark -> String
gameWonBy m = show m <> " WON!"

-- | Parse the input string from the player into game actions.
--
-- >>> parsePlayerCommand "5"
-- Just (5,Nothing)
--
-- >>> parsePlayerCommand "5 RRight"
-- Just (5,Just RRight)
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
    board :: Board
  }

-- | Create the initial game state based on cli flags.
newGameState :: [Flags] -> GameState
newGameState flags
  | CpuVsCpu `elem` flags = GameState X Cpu Cpu newBoard
  | HumanVsHuman `elem` flags = GameState X Human Human newBoard
  | otherwise = GameState X Human Cpu newBoard

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
        False -> gameloop $ GameState next px po board''
        True -> putStrLn $ "GAME OVER! " <> show current <> " Won!"
    Nothing -> putStrLn "GAME OVER! Illigial move"

-- | Make a move on the board based on whether the current player is a human or cpu.
makeMove' :: Board -> Mark -> PlayerType -> IO (Maybe Board)
makeMove' board mark Cpu = return <$> makeMoveCpu board mark
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
    Just r -> rotateBoard board' r
    Nothing -> board'

-- | Make a move on the board by ranomly choosing one of the currently possible moves.
makeMoveCpu :: Board -> Mark -> IO Board
makeMoveCpu board mark = do
  let moves = getPossibleMoves board
  -- Pick a random move out of all the remaining possible moves
  move <- (!!) moves <$> randomRIO (0, length moves - 1)
  return $ setMark board mark move

-- | Command line flags for the program.
data Flags = CpuVsCpu | HumanVsHuman | Help
  deriving (Eq, Ord, Enum, Show, Bounded)

-- | Defines what flags the program accepts.
flags :: [OptDescr Flags]
flags =
  [ Option [] ["cpu"] (NoArg CpuVsCpu) "Run the game in cpu vs. cpu mode",
    Option [] ["human"] (NoArg HumanVsHuman) "Run the game in human vs. ''human mode",
    Option ['h'] ["help"] (NoArg Help) "Print this help message"
  ]

-- | Parse the raw command line arguments into flags.
parse :: [String] -> IO [Flags]
parse argv =
  case getOpt Permute flags argv of
    (f, _, []) -> do
      -- Validate flags
      when (HumanVsHuman `elem` f && CpuVsCpu `elem` f) $ do
        putStrLn "Can only run in cpu vs. cpu OR human vs. human mode"
        putStrLn (usageInfo header flags)
        exitWith $ ExitFailure 400

      -- Print help and exit if --help was passed
      when (Help `elem` f) $ do
        putStrLn (usageInfo header flags)
        exitWith ExitSuccess

      return f
    (_, _, es) -> ioError (userError (concat es ++ usageInfo header flags))
  where
    header = "Usage: tic-tack-roll [--cpu | --human | --help]"

main :: IO ()
main = do
  flags <- getArgs >>= parse

  let gs = newGameState flags
  gameloop gs
