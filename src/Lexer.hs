{- HLINT ignore "Use camelCase" -}
module Lexer where

import Grammar
import Data.Char
import Control.Applicative
import ParserLib (Parser)
import qualified ParserLib as P

data Token = ConstI_T Integer
           | ConstD_T Double
           | Binop_T Binop
           | LParen_T 
           | RParen_T
           deriving (Show, Eq)

whiteSpace = P.whiteSpace

integer :: Parser Integer
integer = whiteSpace >> P.int

double :: Parser Double
double = do
  whiteSpace
  intPart <- P.int <|> return 0
  P.string "."
  decPart <- P.int
  return $ read $ show intPart ++ "." ++ show decPart

string :: String -> Parser String
string s = whiteSpace >> P.string s

char :: Char -> Parser Char
char c = whiteSpace >> P.char c

token :: Parser Token
token =  (double >>= \d -> return (ConstD_T d))
     <|> (integer >>= \i -> return (ConstI_T i))
     <|> (char '+' >> return (Binop_T ADD))
     <|> (char '-' >> return (Binop_T SUB))
     <|> (char '*' >> return (Binop_T MUL))
     <|> (char '/' >> return (Binop_T DIV))
     <|> (char '^' >> return (Binop_T EXP))
     <|> (char '%' >> return (Binop_T MOD))
     <|> (char '(' >> return LParen_T)
     <|> (char ')' >> return RParen_T)

tokens :: Parser [Token]
tokens = P.many token

tokenizeExpr :: String -> [Token]
tokenizeExpr s = P.parse s tokens