module Main where

import Lib (Board, Mark (Empty, O, X), Rotation, getMark, newBoard, rotateBoard, setMark)
import Text.Read (readMaybe)

gameOverStr = "GAME OVER!" :: String

-- | Maybe get the head of a list.
headM :: [a] -> Maybe a
headM [] = Nothing
headM (x : _) = Just x

-- | Maybe get the tail of a list.
tailM :: [a] -> Maybe [a]
tailM [] = Nothing
tailM (_ : xs) = Just xs

-- | Parse the input string from the player into game actions.
--
-- >>> parsePlayerCommand "5"
-- Just (5,Nothing)
--
-- >>> parsePlayerCommand "5 Right"
-- Just (5,Just "Right")
--
-- >>> parsePlayerCommand "this is not valid"
-- Nothing
parsePlayerCommand :: String -> Maybe (Int, Maybe Rotation)
parsePlayerCommand s =
  let cmd = words s
      rotation = tailM cmd >>= headM >>= readMaybe
      index = headM cmd >>= readMaybe
   in (\i -> (i, rotation)) <$> index

makeMove :: Board -> Int -> Either String Board
makeMove b i =
  let p = getMark b i
   in if p /= Empty
        then Left "Mark already present!"
        else Right $ setMark b X i

gameloop :: Board -> IO ()
gameloop b = do
  input <- getLine

  -- Parse the players input
  case parsePlayerCommand input of
    Just (index, Nothing) -> gameloop b
    Just (index, Just rotation) -> do
      let b' = rotateBoard b rotation
      print b'
      case makeMove b' index of
        Right b -> do
          print b
          gameloop b
        Left err -> putStrLn err >> putStrLn gameOverStr
    Nothing -> putStrLn gameOverStr

main :: IO ()
main = do
  let board = newBoard
  gameloop board
