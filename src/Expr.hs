module Expr where

import Parsing

type Name = String

-- At first, 'Expr' contains only addition, conversion to strings, and integer
-- values. You will need to add other operations, and variables
data Expr = Add Expr Expr
          | Subtract Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | ToString Expr
          | Val Value
  deriving Show

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | Quit
  deriving Show


data Value = IntVal Int | StrVal String | CharVal Char | VarVal Name
  deriving Show


setVar :: Name -> Expr -> Command
setVar name value = Set name value



eval :: [(Name, Value)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Value -- Result (if no errors such as missing variables)
eval vars (Val x) = Just x -- for values, just give the value directly
eval vars (Add x y) = do
                        let var1 = eval vars x
                        let var2 = eval vars y

                        case (var1, var2) of
                             (Just (IntVal a), Just (IntVal b)) -> Just (IntVal (a + b))




eval vars (Subtract x y) = Nothing
eval vars (Mul x y) = Nothing
eval vars (Div x y) = Nothing
eval vars (ToString x) = Nothing

digitToInt :: Char -> Value
digitToInt x = IntVal (fromEnum x - fromEnum '0')

pCommand :: Parser Command
pCommand = do t <- ident
              char '='
              e <- pExpr
              return (Set t e)
            ||| do string "print"
                   space
                   e <- pExpr
                   return (Print e)
                   ||| do string "quit"
                          return (Quit)

pExpr :: Parser Expr
pExpr = do t <- pTerm
           do char '+'
              e <- pExpr
              return (Add t e)
            ||| do char '-'
                   e <- pExpr
                   error "Subtraction not yet implemented!"
                 ||| return t

-- pFactor :: Parser Expr
-- pFactor = do d <- digit
--              return (Val (digitToInt d))
--            ||| do v <- letter
--                   return (Val (VarVal [v]))
--                 ||| do char '('
--                        e <- pExpr
--                        char ')'
--                        return e

pFactor :: Parser Expr
pFactor = do d <- natural
             return (Val (IntVal d))
           ||| do v <- identifier
                  return (Val (VarVal v))
                ||| do char '\"'
                       e <- pExpr
                       char '\"'
                       return e
                      ||| do char '('
                             e <- pExpr
                             char ')'
                             return e

pTerm :: Parser Expr
pTerm = do f <- pFactor
           do char '*'
              t <- pTerm
              error "Multiplication not yet implemented"
            ||| do char '/'
                   t <- pTerm
                   error "Division not yet implemented"
                 ||| return f
