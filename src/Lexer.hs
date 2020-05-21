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