{- HLINT ignore "Use camelCase" -}
module Grammar where

data Binop = ADD | SUB | MUL | DIV | EXP | MOD
  deriving (Show, Eq)

data Expression = Binop_E Binop Expression Expression
                | ConstD_E Double
                | ConstI_E Integer
                | Neg_E Expression
                deriving (Show, Eq)

data OP = AddO | SubO | MulO | DivO | ExpO | ModO | ParO | NegO
  deriving (Show, Eq)

binop2Op :: Binop -> OP
binop2Op b
  | b == ADD = AddO
  | b == SUB = SubO
  | b == MUL = MulO
  | b == DIV = DivO
  | b == EXP = ExpO
  | b == MOD = ModO

op2Binop :: OP -> Binop
op2Binop b
  | b == AddO = ADD
  | b == SubO = SUB
  | b == MulO = MUL
  | b == DivO = DIV
  | b == ExpO = EXP
  | b == ModO = MOD
  | otherwise = error $ "CANNOT CONVERT " ++ show b ++ " TO BINOP"