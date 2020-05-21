module Evaluator where

import Grammar
import Data.Fixed (mod')
import Lexer (tokenizeExpr)
import Parser (parseExpr)

data Val = IntVal Integer | DubVal Double
  deriving Eq

instance Show Val where
  show v = case v of
    IntVal i -> show i
    DubVal d -> show d

evalExpr :: String -> Val
evalExpr = evaluate . parseExpr . tokenizeExpr

evalExprDub :: String -> Double
evalExprDub s = case evalExpr s of
  IntVal n -> fromInteger n
  DubVal n -> n

evalExprInt :: String -> Integer
evalExprInt s = case evalExpr s of
  IntVal n -> n
  DubVal n -> floor n

evaluate :: Expression -> Val
evaluate expr = case expr of
  ConstI_E n  -> IntVal n
  ConstD_E n  -> DubVal n
  Neg_E expr1 ->
    case evaluate expr1 of
      DubVal n -> DubVal (-n)
      IntVal n    -> IntVal (-n)
  Binop_E op expr1 expr2 ->
    let 
      (a, b) = case (evaluate expr1, evaluate expr2) of
        (IntVal n1, IntVal n2) -> (IntVal n1, IntVal n2)
        (IntVal n1, DubVal n2) -> (DubVal (fromInteger n1), DubVal n2)
        (DubVal n1, IntVal n2) -> (DubVal n1, DubVal (fromInteger n2))
        (DubVal n1, DubVal n2) -> (DubVal n1, DubVal n2)
    in
      case op of
        ADD ->
          case (a, b) of
            (DubVal n1, DubVal n2) -> DubVal (n1 + n2)
            (IntVal n1, IntVal n2) -> IntVal (n1 + n2)
        SUB ->
          case (a, b) of
            (DubVal n1, DubVal n2) -> DubVal (n1 - n2)
            (IntVal n1, IntVal n2) -> IntVal (n1 - n2)
        MUL ->
          case (a, b) of
            (DubVal n1, DubVal n2) -> DubVal (n1 * n2)
            (IntVal n1, IntVal n2) -> IntVal (n1 * n2)
        DIV ->
          case (a, b) of
            (DubVal n1, DubVal n2) -> DubVal (n1 / n2)
            (IntVal n1, IntVal n2) -> DubVal (fromInteger n1 / fromInteger n2)
        MOD ->
          case (a, b) of
            (DubVal n1, DubVal n2) -> DubVal (n1 `mod'` n2)
            (IntVal n1, IntVal n2) -> IntVal (n1 `mod` n2)
        EXP ->
          case (a, b) of
            (DubVal n1, DubVal n2) -> DubVal (n1 ** n2)
            (IntVal n1, IntVal n2) -> IntVal (n1 ^ n2)