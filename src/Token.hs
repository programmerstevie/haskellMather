{- HLINT ignore "Use camelCase" -}
module Token where

import Grammar

data Token
  = ConstI_T Integer
  | ConstD_T Double
  | Binop_T Binop
  | LParen_T
  | RParen_T
  deriving (Show, Eq)