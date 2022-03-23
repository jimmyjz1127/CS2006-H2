module BinaryTree  where

{-
some data type definitions (originally in Expr.hs) -------------------------------------------------
-}
type Name = String

-- At first, 'Expr' contains only addition, conversion to strings, and integer
-- values. You will need to add other operations, and variables
data Expr = Add Expr Expr
          | Subtract Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | ToString Expr
          | ToInt Expr
          | ToFloat Expr
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
             | Write Expr Expr
             | Quit
             | IfThen Expr [Command]
  deriving Show


data Value = IntVal Int | StrVal String | CharVal Char | VarVal Name | BoolVal Bool | FloatVal Float
  deriving Show


{-
BinaryTree implementation below -----------------------------------------------------------------------
-}

data Node = Node { label :: Name,  -- name of the variable
                   value :: Value, -- the value of the variable
                   ntype  :: String, -- node or null
                   right :: Node,  -- right child node
                   left  :: Node } -- left child node




-- given a (tree) node, name, and value, addValue either updates the corresponding key-value node with the new name and value,
-- or adds a new node containing the key-value to the (tree) node
addValue ::  Node -> Name -> Value -> Node
addValue current name newvalue             = do
                                               case (ntype current) of
                                                 "null"   -> Node name newvalue "node" (Node undefined undefined "null" undefined undefined) (Node undefined undefined "null" undefined undefined)
                                                 "node"   -> do
                                                               if (name > (label current))
                                                                 then do
                                                                        current {right = (addValue (right current) name newvalue)}
                                                               else if (name < (label current))
                                                                 then do
                                                                        current {left = (addValue (left current) name newvalue)}
                                                               else do
                                                                      current {value = newvalue}

-- given a (tree) node and variable name, getValue returns the corresponding value, or undefined String if the variable name cannot be found in the (tree) node
getVal :: Node -> Name -> Value
getVal current name                       = do
                                              case (ntype current) of
                                                "node"    ->  do
                                                                if (name > (label current))
                                                                  then getVal (right current) name
                                                                else if (name < (label current))
                                                                  then getVal (left current) name
                                                                else
                                                                  (value current)
                                                _          -> StrVal "undefined"
