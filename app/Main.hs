module Main where

import Common
import Control.Monad (when)
import Lib
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitSuccess), exitWith)
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

makePlayerMove :: Board -> Int -> Maybe Rotation -> Either String Board
makePlayerMove b i r = do
  b' <- makeMove b X i
  return $ case r of
    Just r' -> rotateBoard b' r'
    Nothing -> b'

makeAIMove :: Board -> IO Board
makeAIMove b = do
  let moves = getPossibleMoves b
  target <- randomRIO (0, length moves - 1)
  let move = moves !! target
  return $ setMark b O move

gameloop :: Board -> IO ()
gameloop b = do
  input <- getLine

  -- Parse the players input
  case parsePlayerCommand input of
    Just (index, rotation) -> do
      case makePlayerMove b index rotation of
        Right b -> do
          if checkIfWon b X
            then putStrLn $ gameWonBy X
            else do
              b' <- makeAIMove b
              print b'
              gameloop b'
        Left err -> putStrLn err >> putStrLn gameOverStr
    Nothing -> putStrLn gameOverStr

-- | Command line flags for the program.
data Flags = CpuVsCpu | Help
  deriving (Eq, Ord, Enum, Show, Bounded)

-- | Defines what flags the program accepts.
flags :: [OptDescr Flags]
flags =
  [ Option [] ["cpu"] (NoArg CpuVsCpu) "Run the game in cpu vs cpu mode",
    Option [] ["help"] (NoArg Help) "Print this help message"
  ]

-- | Parse the raw command line arguments into flags.
parse :: [String] -> IO [Flags]
parse argv =
  case getOpt Permute flags argv of
    (f, _, []) -> do
      -- Print help and exit if --help was passed
      if Help `elem` f
        then do
          putStrLn (usageInfo header flags)
          exitWith ExitSuccess
        else return f
    (_, _, es) -> ioError (userError (concat es ++ usageInfo header flags))
  where
    header = "Usage: tic-tack-roll [--cpu | --help]"

main :: IO ()
main = do
  flags <- getArgs >>= parse

  when (CpuVsCpu `elem` flags) $ putStrLn "Hei"

  let board = newBoard
  print board
  gameloop board
