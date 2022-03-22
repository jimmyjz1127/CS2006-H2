module BinaryTree where

import Expr
import Parsing

-- data TreeNode = Leaf Node | NormalNode Node
--
-- -- data LeafNode = LeafNode { label :: Name,
-- --                            value :: Value }
-- --     deriving Eq
--
-- data Node = Node { label :: Name,  -- name of the variable
--                    value :: Value, -- the value of the variable
--                    right :: TreeNode,  -- right child node
--                    left  :: TreeNode } -- left child node
--     deriving Eq
--
-- -- args : (Root node, parent node, current node, variable name, variable value), returns root node
-- addNode :: TreeNode -> TreeNode -> TreeNode -> Name -> Value -> TreeNode
-- addNode
-- addNode root parent treenode n v = do
--                                      let p = parent
