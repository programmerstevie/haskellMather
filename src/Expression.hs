module Expression where

import Token
import Grammar


data Associativity = LeftA | RightA deriving (Show, Eq)

getAssoc :: OP -> Associativity
getAssoc op
  | op == ExpO = RightA
  | otherwise  = LeftA

opPrecedence :: OP -> Int
opPrecedence ParO = 0
opPrecedence AddO = 1
opPrecedence SubO = 1
opPrecedence MulO = 2
opPrecedence DivO = 2
opPrecedence ModO = 2
opPrecedence ExpO = 3
opPrecedence NegO = 4

opArgs :: OP -> Int
opArgs ParO = 0
opArgs AddO = 1
opArgs SubO = 1
opArgs MulO = 1
opArgs DivO = 1
opArgs ModO = 1
opArgs ExpO = 1
opArgs NegO = 0



getExpr :: [Token] -> Expression
getExpr tokens = case getExpr' tokens [] [] of
                    [expr] -> expr
                    expr   -> tooManyParamsErr
  where
    putOp :: OP -> [Expression] -> [OP] -> [Expression]
    putOp op [] _ = paramErr op
    putOp op [exp] _
      | op == NegO = [Neg_E exp]
      | otherwise  = 
          case op of
            SubO -> [Neg_E exp]
            _    -> prefixErr op
    putOp op expStack@(exp1:exp2:exps) opStack
      | op == ParO = parenErr
      | op == NegO = Neg_E exp1 : (exp2:exps)
      | otherwise = 
          if sum (map opArgs opStack) >= length expStack
          then case op of
                SubO -> Neg_E exp1 : (exp2:exps)
                _    -> prefixErr op
          else Binop_E (op2Binop op) exp2 exp1 : exps

    getExpr' :: [Token] -> [OP] -> [Expression] -> [Expression]
    getExpr' [] [] exprStack = exprStack
    getExpr' [] (op:opStack) exprStack =
        getExpr' [] opStack $ putOp op exprStack  opStack
    getExpr' (t:tokens) opStack exprStack =
      case t of
        ConstD_T n  -> getExpr' tokens opStack $ ConstD_E n : exprStack
        ConstI_T n  -> getExpr' tokens opStack $ ConstI_E n : exprStack
        LParen_T    -> getExpr' tokens (ParO : opStack) exprStack
        RParen_T    -> 
          case opStack of
            [] -> parenErr
            (op:ops) -> 
              if op == ParO 
              then getExpr' tokens ops exprStack
              else getExpr' (t:tokens) ops  $ putOp op exprStack opStack
        Binop_T bop ->
          let currOp = binop2Op bop
          in
            if sum (map opArgs opStack) >= length exprStack -- Prefix Operator
            then case currOp of
              SubO -> getExpr' tokens (NegO : opStack) exprStack
              _    -> prefixErr currOp
            else 
              case opStack of
                [] -> getExpr' tokens [binop2Op bop] exprStack
                (op:ops) ->
                  let topPrec  = opPrecedence op
                      currPrec = opPrecedence currOp
                      currAssoc = getAssoc currOp
                  in if   (topPrec > currPrec
                          ||  (topPrec == currPrec 
                              && currAssoc == LeftA
                              )
                          )
                      &&  (op /= ParO)
                    then getExpr' (t:tokens) ops $ putOp op exprStack opStack  
                    else getExpr' tokens (currOp:opStack) exprStack
                    

tooManyParamsErr :: a
tooManyParamsErr = error $ "[PARSE ERROR] trying to use an "
                        ++ "operation for more parameters than it supports"

parenErr :: a
parenErr = error "[PARSE ERROR] mismatched parentheses"

paramErr :: OP -> a
paramErr op = error $ "[PARSE ERROR] operator >> " 
                   ++ show op ++ " << with no parameters"

prefixErr :: OP -> a
prefixErr op = error $ "[PARSE ERROR] cannot use >> "
                    ++ show op ++ " << as prefix operator"