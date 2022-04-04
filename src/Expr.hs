module Expr where

import Parsing
import BinaryTree


getValueEx :: String ->  Node -> Value
getValueEx name vars = getVal vars name



eval :: Node -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Value -- Result (if no errors such as missing variables)
eval vars (Val x) = do
                      case (x) of
                        (VarVal val) -> Just (getValueEx val vars)
                        _            -> Just x


-- For concatenating strings
eval vars (Concat x y) = do
                            let var1 = eval vars x
                            let var2 = eval vars y

                            case (var1, var2) of
                                 ((Just (StrVal a)),(Just (StrVal b))) -> Just (StrVal (a++b))
                                 _                                     -> Just (StrVal "Type Error")

eval vars (Add x y) = do
                        let var1 = eval vars x
                        let var2 = eval vars y
                        case (var1, var2) of
                             (Just (IntVal a), Just (IntVal b))       -> Just (IntVal (a + b))
                             (Just (IntVal a), Just (FloatVal b))     -> Just (FloatVal (fromIntegral a +  b))
                             (Just (FloatVal a), Just (IntVal b))     -> Just (FloatVal ( a + fromIntegral b))
                             (Just (FloatVal a), Just (FloatVal b))   -> Just (FloatVal ( a +  b))
                             _                                        -> Just (StrVal "Type Error")

eval vars (Subtract x y) = do
                             let var1 = eval vars x
                             let var2 = eval vars y

                             case (var1, var2) of
                                  (Just (IntVal a), Just (IntVal b)) -> Just (IntVal (a - b))
                                  (Just (IntVal a), Just (FloatVal b))     -> Just (FloatVal (fromIntegral a -  b))
                                  (Just (FloatVal a), Just (IntVal b))     -> Just (FloatVal ( a - fromIntegral b))
                                  (Just (FloatVal a), Just (FloatVal b))   -> Just (FloatVal ( a -  b))
                                  _                                  -> Just (StrVal "Type Error")

eval vars (Mul x y) = do
                        let var1 = eval vars x
                        let var2 = eval vars y

                        case (var1, var2) of
                             (Just (IntVal a), Just (IntVal b)) -> Just (IntVal (a * b))
                             (Just (IntVal a), Just (FloatVal b))     -> Just (FloatVal (fromIntegral a *  b))
                             (Just (FloatVal a), Just (IntVal b))     -> Just (FloatVal ( a * fromIntegral b))
                             (Just (FloatVal a), Just (FloatVal b))   -> Just (FloatVal ( a *  b))
                             _                                  -> Just (StrVal "Type Error")

eval vars (Div x y) = do
                        let var1 = eval vars x
                        let var2 = eval vars y

                        case (var1, var2) of
                             (Just (IntVal a), Just (IntVal b)) -> do if b /= 0
                                                                        then Just (IntVal (a `div` b))
                                                                      else Just (StrVal ("Div by 0 error"))

                             (Just (IntVal a), Just (FloatVal b)) -> do if b /= 0
                                                                          then Just (FloatVal (fromIntegral a /  b))
                                                                        else Just (StrVal ("Div by 0 error"))

                             (Just (FloatVal a), Just (IntVal b)) -> do if b /= 0
                                                                          then Just (FloatVal ( a / fromIntegral b))
                                                                        else Just (StrVal ("Div by 0 error"))

                             (Just (FloatVal a), Just (FloatVal b)) -> do if b /= 0
                                                                             then Just (FloatVal ( a /  b))
                                                                          else Just (StrVal ("Div by 0 error"))

                             _                                  -> Just (StrVal "Type Error")

eval vars (Mod x y) = do
                        let var1 = eval vars x
                        let var2 = eval vars y

                        case (var1, var2) of
                             (Just (IntVal a), Just (IntVal b)) -> do if b /= 0
                                                                         then Just (IntVal (a `mod` b))
                                                                      else Just (StrVal ("Div by 0 error"))
                             _                                 -> Just (StrVal ("Type Error"))



eval vars (Pow x y) = do
                        let var1 = eval vars x
                        let var2 = eval vars y

                        case (var1, var2) of
                             (Just (IntVal a), Just (IntVal b))     -> Just (FloatVal ((fromIntegral a) ** (fromIntegral b)))
                             (Just (FloatVal a), Just (IntVal b))   -> (Just (FloatVal (a**fromIntegral b)))
                             (Just (IntVal a), Just (FloatVal b))   -> (Just (FloatVal (fromIntegral a**b)))
                             (Just (FloatVal a), Just (FloatVal b)) -> (Just (FloatVal (a**b)))
                             _                                  -> Just (StrVal "Type Error")

eval vars (ABS x) = do
                        let var1 = eval vars x
                        case var1 of
                             Just (IntVal val1) -> do if val1 < 0
                                                        then Just(IntVal (-val1))
                                                      else Just(IntVal val1)
                             Just (FloatVal val1) -> do if val1 < 0
                                                          then Just(FloatVal (-val1))
                                                        else Just(FloatVal val1)
                             _            -> Just (StrVal "Type Error")

eval vars (CirA x) = do
                        let var1 = eval vars x
                        case var1 of
                             Just (IntVal val1)   -> Just (FloatVal (pi * (fromIntegral val1**2)) )
                             Just (FloatVal val1) -> Just (FloatVal ((pi * (val1**2))) )
                             _            -> Just (StrVal "Type Error")

eval vars (Swap x) = do
                        let var1 = eval vars x
                        case var1 of
                             Just (IntVal val1)   -> Just (FloatVal (fromIntegral val1))
                             Just (FloatVal val1) -> Just (IntVal ( round val1) )
                             _            -> Just (StrVal "Type Error")


eval vars (ToString x) = do
                            let var1 = eval vars x
                            case (var1) of
                                 Just (IntVal val1)   -> Just (StrVal (show val1))
                                 Just (FloatVal val1) -> Just (StrVal (show val1))
                                 _                    -> Just (StrVal ("Type Error"))

eval vars (ToInt x) = do
                            let var1 = eval vars x
                            case (var1) of
                                 Just (StrVal val1)  -> Just (IntVal  (round (read val1)))
                                 _                   -> Just (StrVal ("Type Error"))

eval vars (ToFloat x) = do
                            let var1 = eval vars x
                            case (var1) of
                                 Just (StrVal val1)  -> Just (FloatVal (read val1 :: Float))
                                 _                   -> Just (StrVal ("Type Error"))

-- for boolean expressions involving operators : >, <, <=, >=, ==, /=
eval vars (Compare o x y) = do
                             let var1 = eval vars x
                             let var2 = eval vars y
                             case o of
                               ">" -> case (var1, var2) of
                                 (Just (IntVal a), Just (IntVal b)) -> Just (BoolVal (a > b))
                                 (Just (FloatVal a), Just (FloatVal b)) -> Just (BoolVal (a > b))
                                 (Just (StrVal a), Just (StrVal b)) -> Just (BoolVal (a > b))
                                 _                                        -> Just (StrVal "Type Error")
                               "<" -> case (var1, var2) of
                                 (Just (IntVal a), Just (IntVal b)) -> Just (BoolVal (a < b))
                                 (Just (FloatVal a), Just (FloatVal b)) -> Just (BoolVal (a < b))
                                 (Just (StrVal a), Just (StrVal b)) -> Just (BoolVal (a < b))
                                 _                                        -> Just (StrVal "Type Error")
                               ">=" -> case (var1, var2) of
                                 (Just (IntVal a), Just (IntVal b)) -> Just (BoolVal (a >= b))
                                 (Just (FloatVal a), Just (FloatVal b)) -> Just (BoolVal (a >= b))
                                 (Just (StrVal a), Just (StrVal b)) -> Just (BoolVal (a >= b))
                                 _                                        -> Just (StrVal "Type Error")
                               "<=" -> case (var1, var2) of
                                 (Just (IntVal a), Just (IntVal b)) -> Just (BoolVal (a <= b))
                                 (Just (FloatVal a), Just (FloatVal b)) -> Just (BoolVal (a <= b))
                                 (Just (StrVal a), Just (StrVal b)) -> Just (BoolVal (a <= b))
                                 _                                        -> Just (StrVal "Type Error")
                               "==" -> case (var1, var2) of
                                 (Just (IntVal a), Just (IntVal b)) -> Just (BoolVal (a == b))
                                 (Just (FloatVal a), Just (FloatVal b)) -> Just (BoolVal (a == b))
                                 (Just (StrVal a), Just (StrVal b)) -> Just (BoolVal (a == b))
                                 _                                        -> Just (StrVal "Type Error")
                               "/=" -> case (var1, var2) of
                                 (Just (IntVal a), Just (IntVal b)) -> Just (BoolVal (a /= b))
                                 (Just (FloatVal a), Just (FloatVal b)) -> Just (BoolVal (a /= b))
                                 (Just (StrVal a), Just (StrVal b)) -> Just (BoolVal (a /= b))
                                 _                                        -> Just (StrVal "Type Error")
                               _ -> Just (StrVal "Invalid Symbol")



digitToInt :: Char -> Value
digitToInt x = IntVal (fromEnum x - fromEnum '0')

-- for parsing commands : Set, Print, Quit, Read, Write, if-then-else clauses
pCommand :: Parser Command
pCommand = do t <- ident -- Set command
              space
              char '='
              space
              e <- pExpr
              return (Set t e)
            ||| do string "print" -- Print Command
                   space
                   e <- pExpr
                   return (Print e)
                   ||| do string "quit" -- Quit command
                          return (Quit)
                          ||| do string "read" -- reading from files command
                                 space
                                 e <- pExpr
                                 return(Read e)
                                 ||| do string "write" -- Writing to files command
                                        space
                                        e <- pExpr
                                        space
                                        f <- pExpr
                                        return (Write e f)
                                        ||| do string "if" -- for if-then-else construct
                                               space
                                               char '('
                                               condition <- pExpr
                                               char ')'
                                               space
                                               string "then"
                                               space
                                               char '('
                                               action1 <- pCommand
                                               char ')'
                                               space
                                               string "else"
                                               space
                                               char '('
                                               action2 <- pCommand
                                               char ')'
                                               return (IfThen condition [action1, action2])
                                               ||| do string "if" -- for if-then construct
                                                      space
                                                      char '('
                                                      condition <- pExpr
                                                      char ')'
                                                      space
                                                      string "then"
                                                      space
                                                      char '('
                                                      action1 <- pCommand
                                                      char ')'
                                                      return (IfThen condition [action1])

-- for paring expressions
pExpr :: Parser Expr
pExpr = do string "abs(" -- for absolute value expressions
           e <- pExpr
           char ')'
           return (ABS e)
        ||| do w1 <- pFactor -- for string concatenation expressions
               space
               char '+'
               char '+'
               space
               w2 <- pFactor
               return (Concat w1 w2)
               ||| do string "cirA(" -- for calculating area of circle expressions
                      r <- pExpr
                      char ')'
                      return (CirA r)
                      ||| do string "swap(" -- for converting ints to floats and vice versa
                             t <- pExpr
                             char ')'
                             return (Swap t)
                             ||| do string "toString" -- for toString expressions
                                    e <- pFactor
                                    return (ToString e)
                                    ||| do string "toInt" -- for toInt expressions
                                           e <- pFactor
                                           return (ToInt e)
                                           ||| do string "toFloat" -- for toFloat expressions
                                                  e <- pFactor
                                                  return (ToFloat e)
                                                  ||| do t <- pTerm  -- for numerical addition expressions
                                                         do char '+'
                                                            e <- pExpr
                                                            return (Add t e)
                                                            ||| do char '-' -- for numerical subtraction expressions
                                                                   e <- pExpr
                                                                   return (Subtract t e)
                                                                   ||| do o <- boolComparator -- for comparing values : (<,>,<=,>=,==,/=)
                                                                          e <- pExpr
                                                                          return (Compare o t e)
                                                                          ||| return t

-- for parsing factors in terms and expressions
pFactor :: Parser Expr
pFactor = do f <- float -- for float values
             return (Val (FloatVal f))
             ||| do d <- integer -- for integer values
                    return (Val (IntVal d))
                    ||| do v <- identifier -- for variable identifiers
                           return (Val (VarVal v))
                           ||| do w <- stringLit -- for string literals
                                  return (Val (StrVal w))
                                  ||| do char '(' -- for expressions wrapped in parenthesis
                                         e <- pExpr
                                         char ')'
                                         return e
                                         ||| do string "False" -- for boolean value False
                                                return (Val (BoolVal False))
                                                ||| do string "True" -- for boolean value True
                                                       return (Val (BoolVal True))

-- for parsing terms in expressions
pTerm :: Parser Expr
pTerm = do f <- pFactor -- for multiplicative terms
           do char '*'
              t <- pTerm
              return (Mul f t)
            ||| do char '/' -- for division terms
                   t <- pTerm
                   return (Div f t)
                   ||| do char '^' -- for exponential terms
                          char '('
                          t <- pTerm
                          char ')'
                          return (Pow f t)
                          ||| do char '%' -- for modulus terms
                                 t <- pTerm
                                 return (Mod f t)
                          ||| return f
