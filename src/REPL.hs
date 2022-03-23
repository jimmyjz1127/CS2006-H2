module REPL where

import Expr
import Parsing
import BinaryTree
import System.IO
import Control.Exception
import Control.Concurrent


data LState = LState { vars :: Node }


initLState :: LState
--initialize LState with BinaryTree with root node with name/label "0"
initLState = LState (Node "0" undefined "node" (Node undefined undefined "null" undefined undefined) (Node undefined undefined "null" undefined undefined))

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value

updateVars :: Name -> Value -> LState -> LState
updateVars name val st = do
                          let tree = vars st
                          let newtree = addValue tree name val
                          st {vars = newtree}


getValue :: String -> LState -> Value
getValue name st = getVal (vars st) name





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
            Just (FloatVal val)-> putStrLn(show val)
            Just (BoolVal val) -> putStrLn(show val)
            Just (VarVal var)  -> case (getValue var st) of
                 (IntVal val)  -> putStrLn(show val)
                 (CharVal val) -> putStrLn(show val)
                 (StrVal val)  -> putStrLn(val)
                 (FloatVal val)-> putStrLn(show val)
                 (BoolVal val) -> putStrLn(show val)
            Nothing -> putStrLn("")
      repl st

process st (Quit) = putStrLn("Quitting Program...")


--Read content from a text file  line by line and execute
process st (Read e) = do case (eval (vars st) e) of
                              Just (StrVal val) -> do check <- try (readFile val) :: IO (Either SomeException String)
                                                      case check of
                                                           Left err   -> do putStrLn((show err) ++ "\n")  --Error, mainly because of file not exist
                                                                            repl st
                                                           Right pass -> do file <- readFile val          --If pass
                                                                            let content = lines file
                                                                            replf st content
                              _                 -> do putStrLn ("Error")
                                                      repl st

--Read file path and string, append string to the end of the file
process st (Write e f) = do case (eval (vars st) e, eval (vars st) f) of
                                 (Just (StrVal val1), Just (StrVal val2)) -> do check <- try (appendFile val1 (""++val2)) :: IO (Either SomeException () ) --Try to write content to file
                                                                                case check of
                                                                                     Left err   -> do putStrLn("write error \n")  --If error thrown
                                                                                                      repl st
                                                                                     Right pass -> repl st
                                 _                                        -> do putStrLn ("Error")
                                                                                repl st



process st (IfThen c a) = do
                             let var = eval (vars st) c

                             case var of
                               Just (BoolVal x) -> do
                                                     case x of
                                                       True -> process st (head a)
                                                       False -> do
                                                                  case (length a) of
                                                                    2 -> process st (last a)
                                                                    _ -> repl st
                               _                -> putStrLn("Invalid Condition")


--Seperate operation for commands read from file
--It returns to replf instead of repl
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
            Just (FloatVal val)-> putStrLn(show val)
            Just (VarVal var)  -> case (getValue var st) of
                 (IntVal val)  -> putStrLn(show val)
                 (CharVal val) -> putStrLn(show val)
                 (FloatVal val)-> putStrLn(show val)
                 (StrVal val)  -> putStrLn(val)
                 _             -> putStrLn("Error")
            Nothing -> putStrLn("")
            _                  -> putStrLn("Error")
      replf st inp

processf st (IfThen c a) inp =  do
                                let var = eval (vars st) c

                                case var of
                                  Just (BoolVal x) -> do
                                                        case x of
                                                          True -> processf st (head a) inp
                                                          False -> do
                                                                      case (length a) of
                                                                        2 -> process st (last a)
                                                                        _ -> replf st inp
                                  _                -> putStrLn("Invalid Condition")

processf st (Quit) inp = do putStrLn("Quitting Program...")
                            threadDelay 5000000
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



--Similar operation to repl
--It takes in an extra argument which is the line read from the text file
--Similar to repl , it calls 'process' to process the command
-- 'process' will call 'replf' when done
--It loops until the last line is read, then it will call 'repl'
replf :: LState -> [String] -> IO ()
replf st inp = do if (length inp) /= 0
                     then do
                            putStrLn("\nFile: " ++ (head inp)) --Optional
                            case parse pCommand (head inp) of
                                 [(cmd, "")] -> do processf st cmd (tail inp)
                                 _ -> do putStrLn "Parse error"
                                         replf st (tail inp)
                  else repl st
