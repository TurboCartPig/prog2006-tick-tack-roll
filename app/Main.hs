module Main where

import Common
import Lib
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

main :: IO ()
main = do
  let board = newBoard
  print board
  gameloop board
