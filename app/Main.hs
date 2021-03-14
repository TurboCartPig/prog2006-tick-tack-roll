module Main where

import           Board
import           Control.Monad         (when)
import           Game
import           System.Console.GetOpt
import           System.Environment    (getArgs)
import           System.Exit           (ExitCode (ExitFailure, ExitSuccess),
                                        exitWith)

-- | Create the initial game state based on cli flags.
newGameState :: [Flags] -> GameState
newGameState flags
  | CpuVsCpu `elem` flags = GameState X Cpu Cpu newBoard
  | HumanVsHuman `elem` flags = GameState X Human Human newBoard
  | otherwise = GameState X Human Cpu newBoard

-- | Command line flags for the program.
data Flags = CpuVsCpu | HumanVsHuman | Help
  deriving (Eq, Ord, Enum, Show, Bounded)

-- | Defines what flags the program accepts.
flags :: [OptDescr Flags]
flags =
  [ Option [] ["cpu"] (NoArg CpuVsCpu) "Run the game in cpu vs. cpu mode",
    Option [] ["human"] (NoArg HumanVsHuman) "Run the game in human vs. human mode",
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
