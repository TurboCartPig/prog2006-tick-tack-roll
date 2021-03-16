module Main where

import           Board
import           Common
import           Test.DocTest    (doctest)
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = do
  putStrLn "DocTests:"
  doctest ["app", "src"]

  putStr "\nHspec tests:"
  hspec $ do
    describe "Functions applied to the board..." $ do
      it "swaps the corners of the board" $ do
        swapCorners (Board [X, Empty, O, Empty, Empty, Empty, Empty, Empty, Empty])
          `shouldBe` (Board [O, Empty, X, Empty, Empty, Empty, Empty, Empty, Empty])
      it "rotates the board left" $ do
        rotateLeft (Board [O, Empty, O, Empty, Empty, Empty, X, Empty, X])
          `shouldBe` (Board [O, Empty, X, Empty, Empty, Empty, O, Empty, X])
      it "rotates the board right" $ do
        rotateRight (Board [O, Empty, O, Empty, Empty, Empty, X, Empty, X])
          `shouldBe` (Board [X, Empty, O, Empty, Empty, Empty, X, Empty, O])
      it "rotates the board" $ do
        (rotateBoard (Board [O, Empty, O, Empty, Empty, Empty, X, Empty, X]) RRight)
          `shouldBe` (Board [X, Empty, O, Empty, Empty, Empty, X, Empty, O])

    describe "Enumerations..." $ do
      it "enumerates with same length" $ do
        property $ \xs -> length xs == length (enumerate xs :: [(Int, Int)])
