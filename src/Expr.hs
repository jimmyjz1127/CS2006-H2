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
          | Pow Expr Expr
          | Mod Expr Expr
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



getValueEx :: String ->  [(Name, Value)] -> Value
getValueEx name vars = snd(head(filter (\a -> fst(a) == name ) (vars)))




eval :: [(Name, Value)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Maybe Value -- Result (if no errors such as missing variables)
eval vars (Val x) = Just x -- for values, just give the value directly
eval vars (Add x y) = do
                        let var1 = eval vars x
                        let var2 = eval vars y
                        --
                        -- case (var1, var2) of
                        --      (Just (IntVal a), Just (IntVal b)) -> Just (IntVal (a + b))
                        case (var1, var2) of
                             (Just (IntVal a), Just (IntVal b)) -> Just (IntVal (a + b))
                             (Just (VarVal a), Just (IntVal b)) -> do let val1 = getValueEx a vars
                                                                      case val1 of
                                                                          (IntVal val1_1) -> Just (IntVal (val1_1 + b))

                             (Just (IntVal a), Just (VarVal b)) -> do let val2 = getValueEx b vars
                                                                      case val2 of
                                                                          (IntVal val2_2) -> Just (IntVal (a + val2_2))

                             (Just (VarVal a), Just (VarVal b)) -> do let val1 = getValueEx a vars
                                                                      let val2 = getValueEx b vars
                                                                      case (val1,val2) of
                                                                           ((IntVal val1_1),  (IntVal val2_2)) -> Just (IntVal (val1_1 + val2_2))




eval vars (Subtract x y) = do
                             let var1 = eval vars x
                             let var2 = eval vars y

                             case (var1, var2) of
                                  (Just (IntVal a), Just (IntVal b)) -> Just (IntVal (a - b))
                                  (Just (VarVal a), Just (IntVal b)) -> do let val1 = getValueEx a vars
                                                                           case val1 of
                                                                               (IntVal val1_1) -> Just (IntVal (val1_1 - b))

                                  (Just (IntVal a), Just (VarVal b)) -> do let val2 = getValueEx b vars
                                                                           case val2 of
                                                                               (IntVal val2_2) -> Just (IntVal (a - val2_2))

                                  (Just (VarVal a), Just (VarVal b)) -> do let val1 = getValueEx a vars
                                                                           let val2 = getValueEx b vars
                                                                           case (val1,val2) of
                                                                                ((IntVal val1_1),  (IntVal val2_2)) -> Just (IntVal (val1_1 - val2_2))



eval vars (Mul x y) = do
                        let var1 = eval vars x
                        let var2 = eval vars y

                        case (var1, var2) of
                             (Just (IntVal a), Just (IntVal b)) -> Just (IntVal (a * b))
                             (Just (VarVal a), Just (IntVal b)) -> do let val1 = getValueEx a vars
                                                                      case val1 of
                                                                          (IntVal val1_1) -> Just (IntVal (val1_1 * b))

                             (Just (IntVal a), Just (VarVal b)) -> do let val2 = getValueEx b vars
                                                                      case val2 of
                                                                          (IntVal val2_2) -> Just (IntVal (a * val2_2))

                             (Just (VarVal a), Just (VarVal b)) -> do let val1 = getValueEx a vars
                                                                      let val2 = getValueEx b vars
                                                                      case (val1,val2) of
                                                                           ((IntVal val1_1),  (IntVal val2_2)) -> Just (IntVal (val1_1 * val2_2))




eval vars (Div x y) = do
                        let var1 = eval vars x
                        let var2 = eval vars y

                        case (var1, var2) of
                             (Just (IntVal a), Just (IntVal b)) -> do if b /= 0
                                                                        then Just (IntVal (a `div` b))
                                                                      else Just (StrVal ("Div by 0 error"))

                             (Just (VarVal a), Just (IntVal b)) -> do if b /= 0
                                                                         then do let val1 = getValueEx a vars
                                                                                 case val1 of
                                                                                      (IntVal val1_1) -> Just (IntVal (val1_1 `div` b))
                                                                      else Just (StrVal ("Div by 0 error"))



                             (Just (IntVal a), Just (VarVal b)) -> do let val2 = getValueEx b vars
                                                                      case val2 of
                                                                          (IntVal val2_2) -> do if val2_2 /= 0
                                                                                                  then Just (IntVal (a `div` val2_2))
                                                                                                else Just (StrVal ("Div by 0 error"))

                             (Just (VarVal a), Just (VarVal b)) -> do let val1 = getValueEx a vars
                                                                      let val2 = getValueEx b vars
                                                                      case (val1,val2) of
                                                                           ((IntVal val1_1),  (IntVal val2_2)) -> do if val2_2 /= 0
                                                                                                                        then Just (IntVal (val1_1 `div` val2_2))
                                                                                                                     else Just (StrVal ("Div by 0 error"))



eval vars (Mod x y) = do
                        let var1 = eval vars x
                        let var2 = eval vars y

                        case (var1, var2) of
                             (Just (IntVal a), Just (IntVal b)) -> do if b /= 0
                                                                        then Just (IntVal (a `mod` b))
                                                                      else Just (StrVal ("Div by 0 error"))

                             (Just (VarVal a), Just (IntVal b)) -> do if b /= 0
                                                                         then do let val1 = getValueEx a vars
                                                                                 case val1 of
                                                                                      (IntVal val1_1) -> Just (IntVal (val1_1 `mod` b))
                                                                      else Just (StrVal ("Div by 0 error"))



                             (Just (IntVal a), Just (VarVal b)) -> do let val2 = getValueEx b vars
                                                                      case val2 of
                                                                          (IntVal val2_2) -> do if val2_2 /= 0
                                                                                                  then Just (IntVal (a `mod` val2_2))
                                                                                                else Just (StrVal ("Div by 0 error"))

                             (Just (VarVal a), Just (VarVal b)) -> do let val1 = getValueEx a vars
                                                                      let val2 = getValueEx b vars
                                                                      case (val1,val2) of
                                                                           ((IntVal val1_1),  (IntVal val2_2)) -> do if val2_2 /= 0
                                                                                                                        then Just (IntVal (val1_1 `mod` val2_2))
                                                                                                                     else Just (StrVal ("Div by 0 error"))





eval vars (Pow x y) = do
                        let var1 = eval vars x
                        let var2 = eval vars y

                        case (var1, var2) of
                             (Just (IntVal a), Just (IntVal b)) -> Just (IntVal (a^b))
                             (Just (VarVal a), Just (IntVal b)) -> do let val1 = getValueEx a vars
                                                                      case val1 of
                                                                          (IntVal val1_1) -> Just (IntVal (val1_1^b))

                             (Just (IntVal a), Just (VarVal b)) -> do let val2 = getValueEx b vars
                                                                      case val2 of
                                                                          (IntVal val2_2) -> Just (IntVal (a^val2_2))

                             (Just (VarVal a), Just (VarVal b)) -> do let val1 = getValueEx a vars
                                                                      let val2 = getValueEx b vars
                                                                      case (val1,val2) of
                                                                           ((IntVal val1_1),  (IntVal val2_2)) -> Just (IntVal (val1_1^val2_2))








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
                   return (Subtract t e)
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
