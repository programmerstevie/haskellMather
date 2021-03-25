module Parser where

import Grammar
import Control.Applicative ((<|>))
import Lexer (char, integer, double)
import Token (Token(..))
import ParserLib (Parser, many)
import Expression (getExpr)


expression :: Parser Expression
expression = getExpr <$> tokens

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
tokens = many token