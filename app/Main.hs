module Main where

import Evaluator

main :: IO ()
main = do
  msg <- getLine
  print $ evalExprInt msg
