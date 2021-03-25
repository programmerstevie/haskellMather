module Evaluator where

import Data.Fixed (mod')
import Grammar
import Parser (expression)
import ParserLib (parse)

data Val = IntVal Integer | DubVal Double
  deriving (Eq)

instance Show Val where
  show v = case v of
    IntVal i -> show i
    DubVal d -> show d

evalExpr :: String -> Val
evalExpr s = evaluate $ parse s expression

evalExprDub :: String -> Double
evalExprDub s = case evalExpr s of
  IntVal n -> fromInteger n
  DubVal n -> n

evalExprInt :: String -> Integer
evalExprInt s = case evalExpr s of
  IntVal n -> n
  DubVal n -> floor n

simpDub :: Val -> Val
simpDub (DubVal n)
  | floor n == ceiling n = IntVal (floor n)
  | otherwise = DubVal n
simpDub (IntVal n) = IntVal n

dubValSimp :: Double -> Val
dubValSimp = simpDub . DubVal

evaluate :: Expression -> Val
evaluate expr = case expr of
  ConstI_E n -> IntVal n
  ConstD_E n -> simpDub $ DubVal n
  Neg_E expr1 ->
    case evaluate expr1 of
      DubVal n -> simpDub $ DubVal (- n)
      IntVal n -> IntVal (- n)
  Binop_E op expr1 expr2 ->
    let (a, b) = case (evaluate expr1, evaluate expr2) of
          (IntVal n1, IntVal n2) -> (IntVal n1, IntVal n2)
          (IntVal n1, DubVal n2) -> (DubVal (fromInteger n1), DubVal n2)
          (DubVal n1, IntVal n2) -> (DubVal n1, DubVal (fromInteger n2))
          (DubVal n1, DubVal n2) -> (DubVal n1, DubVal n2)
     in case op of
          ADD ->
            case (a, b) of
              (DubVal n1, DubVal n2) -> dubValSimp (n1 + n2)
              (IntVal n1, IntVal n2) -> IntVal (n1 + n2)
          SUB ->
            case (a, b) of
              (DubVal n1, DubVal n2) -> dubValSimp (n1 - n2)
              (IntVal n1, IntVal n2) -> IntVal (n1 - n2)
          MUL ->
            case (a, b) of
              (DubVal n1, DubVal n2) -> dubValSimp (n1 * n2)
              (IntVal n1, IntVal n2) -> IntVal (n1 * n2)
          DIV ->
            case (a, b) of
              (DubVal n1, DubVal n2) -> dubValSimp (n1 / n2)
              (IntVal n1, IntVal n2) -> dubValSimp (fromInteger n1 / fromInteger n2)
          MOD ->
            case (a, b) of
              (DubVal n1, DubVal n2) -> dubValSimp (n1 `mod'` n2)
              (IntVal n1, IntVal n2) -> IntVal (n1 `mod` n2)
          EXP ->
            case (a, b) of
              (DubVal n1, DubVal n2) -> dubValSimp (n1 ** n2)
              (IntVal n1, IntVal n2) -> IntVal (n1 ^ n2)