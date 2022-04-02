import Expr
import REPL
import BinaryTree
import Test.QuickCheck


instance Arbitrary Value where
  arbitrary = oneof [arbitraryInt, arbitraryString, arbitraryChar, arbitraryName, arbitraryBool, arbitraryFloat]
    where arbitraryInt      = do
                                i <- arbitrary
                                return (IntVal i)
          arbitraryString   = do
                                s <- arbitrary
                                return (StrVal s)
          arbitraryChar     = do
                                c <- arbitrary
                                return (CharVal c)
          arbitraryName     = do
                                n <- listOf1 arbitrary
                                return (VarVal n)
          arbitraryBool     = do
                                b <- arbitrary
                                return (BoolVal b)
          arbitraryFloat    = do
                                f <- arbitrary
                                return (FloatVal f)

emptytree = Node undefined undefined undefined undefined undefined

-------------------------------------Testing Basic Atomic Arithemtic Operations --------------------------
prop_simpleAdd :: Value -> Value -> Bool
prop_simpleAdd a b = do
                       case (a,b) of
                         (VarVal i, _) -> True
                         (_, VarVal i) -> True
                         _             -> do
                                           let result1 = eval emptytree (Add (Val a) (Val b))
                                           let result2 = case (a,b) of
                                                             (IntVal x, IntVal y)     -> IntVal (x+y)
                                                             (FloatVal x, FloatVal y) -> FloatVal (x+y)
                                                             (IntVal x, FloatVal y)   -> FloatVal (fromIntegral(x)+y)
                                                             (FloatVal x, IntVal y)   -> FloatVal (x + fromIntegral(y))
                                                             _                        -> StrVal "Type Error"
                                           do
                                             case (result1, result2) of
                                               (Just (IntVal x), IntVal y) -> (x==y)
                                               (Just (FloatVal x), FloatVal y) -> (x==y)
                                               (Just (StrVal x), StrVal y) -> (x==y)




main :: IO ()
main = do
         quickCheck prop_simpleAdd
