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
          | Concat Expr Expr
          | Pow Expr Expr
          | Mod Expr Expr
          | ABS Expr
          | CirA Expr
          | Swap Expr
          | Val Value
          | Compare String Expr Expr

  deriving Show

-- These are the REPL commands
data Command = Set Name Expr -- assign an expression to a variable name
             | Print Expr    -- evaluate an expression and print the result
             | Read Expr
             | Quit
             | IfThen Expr [Command]
  deriving Show


data Value = IntVal Int | StrVal String | CharVal Char | VarVal Name | BoolVal Bool | FloatVal Float
  deriving Show


-- setVar :: Name -> Expr -> Command
-- setVar name value = Set name value



getValueEx :: String ->  [(Name, Value)] -> Value
getValueEx name vars = snd(head(filter (\a -> fst(a) == name ) (vars)))



eval :: [(Name, Value)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Value -- Result (if no errors such as missing variables)
eval vars (Val x) = do
                      case (x) of
                        (VarVal val) -> Just (getValueEx val vars)
                        _            -> Just x


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
                             _                                 -> Just (StrVal ("Type error"))



eval vars (Pow x y) = do
                        let var1 = eval vars x
                        let var2 = eval vars y

                        case (var1, var2) of
                             (Just (IntVal a), Just (IntVal b))     -> Just (IntVal (a^b))
                             (Just (FloatVal a), Just (IntVal b))   -> Just (FloatVal (a^b))
                             (Just (IntVal a), Just (FloatVal b))   -> Just (FloatVal (fromIntegral a**b))
                             (Just (FloatVal a), Just (FloatVal b)) -> Just (FloatVal (a**b))
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
                                 Just (IntVal val1)  -> Just (StrVal (show val1))
                                 Just (FloatVal val1)-> Just (StrVal (show val1))

eval vars (Compare o x y) = do
                             let var1 = eval vars x
                             let var2 = eval vars y
                             case o of
                               ">" -> case (var1, var2) of
                                 (Just (IntVal a), Just (IntVal b)) -> Just (BoolVal (a > b))
                                 (Just (StrVal a), Just (StrVal b)) -> Just (BoolVal (a > b))
                                 _                                        -> Just (StrVal "Type Error")
                               "<" -> case (var1, var2) of
                                 (Just (IntVal a), Just (IntVal b)) -> Just (BoolVal (a < b))
                                 (Just (StrVal a), Just (StrVal b)) -> Just (BoolVal (a < b))
                                 _                                        -> Just (StrVal "Type Error")
                               ">=" -> case (var1, var2) of
                                 (Just (IntVal a), Just (IntVal b)) -> Just (BoolVal (a >= b))
                                 (Just (StrVal a), Just (StrVal b)) -> Just (BoolVal (a >= b))
                                 _                                        -> Just (StrVal "Type Error")
                               "<=" -> case (var1, var2) of
                                 (Just (IntVal a), Just (IntVal b)) -> Just (BoolVal (a <= b))
                                 (Just (StrVal a), Just (StrVal b)) -> Just (BoolVal (a <= b))
                                 _                                        -> Just (StrVal "Type Error")
                               "==" -> case (var1, var2) of
                                 (Just (IntVal a), Just (IntVal b)) -> Just (BoolVal (a == b))
                                 (Just (StrVal a), Just (StrVal b)) -> Just (BoolVal (a == b))
                                 _                                        -> Just (StrVal "Type Error")
                               "/=" -> case (var1, var2) of
                                 (Just (IntVal a), Just (IntVal b)) -> Just (BoolVal (a /= b))
                                 (Just (StrVal a), Just (StrVal b)) -> Just (BoolVal (a /= b))
                                 _                                        -> Just (StrVal "Type Error")
                               _ -> Just (StrVal "Invalid Symbol")



digitToInt :: Char -> Value
digitToInt x = IntVal (fromEnum x - fromEnum '0')

pCommand :: Parser Command
pCommand = do t <- ident
              space
              char '='
              space
              e <- pExpr
              return (Set t e)
            ||| do string "print"
                   space
                   e <- pExpr
                   return (Print e)
                   ||| do string "quit"
                          return (Quit)
                          ||| do string "read"
                                 space
                                 e <- pExpr
                                 return(Read e)
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

pExpr :: Parser Expr
pExpr = do string "abs("
           e <- pExpr
           char ')'
           return (ABS e)
        ||| do w1 <- pFactor
               char '+'
               char '+'
               w2 <- pFactor
               return (Concat w1 w2)
               ||| do string "cirA("
                      r <- pExpr
                      char ')'
                      return (CirA r)
                      ||| do string "swap("
                             t <- pExpr
                             char ')'
                             return (Swap t)
                             ||| do string "toString"
                                    e <- pFactor
                                    return (ToString e)
                                  ||| do t <- pTerm
                                         do char '+'
                                            e <- pExpr
                                            return (Add t e)
                                            ||| do char '-'
                                                   e <- pExpr
                                                   return (Subtract t e)
                                                   ||| do o <- boolComparator
                                                          e <- pExpr
                                                          return (Compare o t e)
                                                          ||| return t



-- pFactor :: Parser Expr
-- pFactor = do d <- digit
--              return (Val (digitToInt d))
--            ||| do v <- letter
--                   return (Val (VarVal [v]))
--                        e <- pExpr
--                        char ')'
--                        return e

pFactor :: Parser Expr
pFactor = do f <- float
             return (Val (FloatVal f))
             ||| do d <- integer
                    return (Val (IntVal d))
                    ||| do v <- identifier
                           return (Val (VarVal v))
                           ||| do w <- stringLit
                                  return (Val (StrVal w))
                                  ||| do char '('
                                         e <- pExpr
                                         char ')'
                                         return e


pTerm :: Parser Expr
pTerm = do f <- pFactor
           do char '*'
              t <- pTerm
              return (Mul f t)
            ||| do char '/'
                   t <- pTerm
                   return (Div f t)
                   ||| do char '^'
                          t <- pTerm
                          return (Pow f t)
                          ||| do char '%'
                                 t <- pTerm
                                 return (Mod f t)
                          ||| return f
