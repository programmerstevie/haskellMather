{-# LANGUAGE LambdaCase #-}

module ParserLib where

import Control.Monad.State.Lazy (StateT(StateT), runStateT)
import Control.Monad (MonadPlus, mzero, mplus, liftM, ap, void)
import Control.Applicative (Alternative, empty, (<|>))
import Data.Foldable (asum)
import Data.Char (isDigit)
import Data.List (concatMap)

newtype Parser a = Parser {unParse :: StateT String [] a}

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return  = Parser . return
  p >>= f = Parser $ unParse p >>= (unParse . f)

instance Alternative Parser where
  empty = mzero
  p <|> q = parserT $ \s ->
    case runParser p s of
      [] -> runParser q s
      rs -> rs

instance MonadPlus Parser where
  mzero = parserT $ const []
  mplus p q = parserT $ \s -> runParser p s ++ runParser q s

parse :: String -> Parser a -> a
parse s p
  | null ls        = error "Nothing Read"
  | null (tail ls) = fst . head $ ls
  | otherwise      = error "Multiple Parses!"
    where ls = runParser p s

many :: Parser a -> Parser [a]
many p = manyV
  where manyV = someV <|> return []
        someV = (:) <$> p <*> manyV

some :: Parser a -> Parser [a]
some p = someV
  where manyV = someV <|> return []
        someV = (:) <$> p <*> manyV

runParser :: Parser a -> String -> [(a, String)]
runParser = runStateT . unParse

oneOf :: String -> Parser Char
oneOf cs = asum $ map char cs

int :: Parser Integer
int = read <$> some digit

item :: Parser Char
item = parserT $
  \case
    "" -> []
    (c:cs) -> [(c, cs)]

predP :: (Char -> Bool) -> Parser Char
predP pred = do
  c <- item
  if pred c
  then return c
  else empty

digit :: Parser Char
digit = predP isDigit

string :: String -> Parser String
string = mapM char

char :: Char -> Parser Char
char = predP . (==)

whiteSpace :: Parser ()
whiteSpace = void $ many (oneOf " \n")

parserT :: (String -> [(a, String)]) -> Parser a
parserT = Parser . StateT