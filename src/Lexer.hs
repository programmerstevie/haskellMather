module Lexer where

import Control.Applicative ((<|>))
import Grammar
import ParserLib (Parser)
import qualified ParserLib as P

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