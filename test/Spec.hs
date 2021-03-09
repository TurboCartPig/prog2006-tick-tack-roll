module Main where

import Test.DocTest (doctest)

main :: IO ()
main = do
  doctest ["app", "src"]
