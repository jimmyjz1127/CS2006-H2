module REPL where

import Expr
import Parsing
import System.IO

data LState = LState { vars :: [(Name, Value)] }


initLState :: LState
initLState = LState []

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Value -> LState -> LState
updateVars name value st = do
                              if (contains name st)
                                   then do
                                        let tempState = dropVar name st
                                        let currentList = (vars tempState) ++ [(name, value)]
                                        tempState {vars = currentList}
                              else do
                                        let currentList = vars st
                                        st {vars = currentList ++ [(name, value)]}



contains :: Name ->  LState -> Bool
contains name st = length (filter (\a -> fst(a) == name ) (vars st)) > 0

getValue :: String ->  LState -> Value
getValue name st = do if contains name st
                        then snd(head(filter (\a -> fst(a) == name ) (vars st)))
                      else (StrVal)("Value not found")
-- Return a new set of variables with the given name removed
dropVar :: Name -> LState -> LState
dropVar name st = do
                    let tempList = (filter (\a -> fst(a) /= name) (vars st))
                    st{vars = tempList}





process :: LState -> Command -> IO ()
process st (Set name e) =
  case (eval (vars st) e) of
    Just (VarVal val) -> repl (updateVars name (getValue val st) st)
    Just val -> repl (updateVars name val st)
    Nothing  -> repl st
process st (Print e) =
    do
      case (eval (vars st) e) of
            Just (IntVal val)  -> putStrLn(show val)
            Just (StrVal val)  -> putStrLn(val)
            Just (CharVal val) -> putStrLn(show val)
            Just (BoolVal val) -> putStrLn(show val)
            Just (VarVal var)  -> case (getValue var st) of
                 (IntVal val)  -> putStrLn(show val)
                 (CharVal val) -> putStrLn(show val)
                 (StrVal val)  -> putStrLn(val)
                 (BoolVal val) -> putStrLn(show val)
            Nothing -> putStrLn("")
      repl st
process st (Quit) = putStrLn("Quitting Program...")


process st (Read e) = do case (eval (vars st) e) of
                              Just (StrVal val) -> do file <- readFile val
                                                      let content = lines file
                                                      replf st content
                              _                 -> do putStrLn ("Error")
                                                      repl st

--------------------------------------------------------------------------------
processf :: LState -> Command  -> [String] -> IO ()
processf st (Set name e)inp =
  case (eval (vars st) e) of
    Just (VarVal val) -> replf (updateVars name (getValue val st) st) inp
    Just val -> replf (updateVars name val st) inp
    Nothing  -> replf st inp


processf st (Print e) inp =
    do
      case (eval (vars st) e) of
            -- Just (NameVal name) -> putStrLn(show (getValue name st))
            Just (IntVal val)  -> putStrLn(show val)
            Just (StrVal val)  -> putStrLn(val)
            Just (CharVal val) -> putStrLn(show val)
            Just (VarVal var)  -> case (getValue var st) of
                 (IntVal val)  -> putStrLn(show val)
                 (CharVal val) -> putStrLn(show val)
                 (StrVal val)  -> putStrLn(val)
            Nothing -> putStrLn("")
      replf st inp

processf st (Quit) inp = putStrLn("Quitting Program...")
--------------------------------------------------------------------------------



-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: LState -> IO ()
repl st = do putStr ("> ")
             inp <- getLine
             case parse pCommand inp of
                  [(cmd, "")] -> -- Must parse entire input
                          process st cmd
                  _ -> do putStrLn "Parse error"
                          repl st


replf :: LState -> [String] -> IO ()
replf st inp = do if (length inp) /= 0
                     then do
                            putStrLn("Line: " ++ (head inp)) --Remove
                            case parse pCommand (head inp) of
                                 [(cmd, "")] -> do processf st cmd (tail inp)
                                 _ -> do putStrLn "Parse error"
                  else repl st
