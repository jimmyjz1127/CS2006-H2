module BinaryTree where

import Expr
import Parsing


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
getValue :: Node -> Name -> Value
getValue current name                     = do
                                              case (ntype current) of
                                                "node"    ->  do
                                                                if (name > (label current))
                                                                  then getValue (right current) name
                                                                else if (name < (label current))
                                                                  then getValue (left current) name
                                                                else
                                                                  (value current)
                                                _          -> StrVal "undefined"


addNode :: Node -> Node -> Node
addNode current new =                       do
                                              case (ntype current) of
                                                "node"    -> do
                                                               if ((label new) > (label current))
                                                                 then do
                                                                        current {right = (addNode (right current) new)}
                                                               else if ((label new) < (label current))
                                                                 then do
                                                                        current {left = (addNode (left current) new)}
                                                               else do
                                                                      let currentright = right current
                                                                      let currentleft = left current
                                                                      new {right = currentright, left = currentleft}
                                                "null"    -> new
