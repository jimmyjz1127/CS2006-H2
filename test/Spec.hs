import Expr
import REPL
import BinaryTree
import Test.QuickCheck
import Data.Typeable


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

-- Tests basic addition between two atomic values
prop_simpleAdd :: Value -> Value -> Bool
prop_simpleAdd a b = do
                       case (a,b) of
                         (VarVal i, _) -> True --ignore var val since we aren't dealing with Lstate
                         (_, VarVal i) -> True --ignore var val since we aren't dealing with Lstate
                         _             -> do
                                           let result1 = eval emptytree (Add (Val a) (Val b))
                                           let result2 = case (a,b) of
                                                             (IntVal x, IntVal y)     -> IntVal (x+y)
                                                             (FloatVal x, FloatVal y) -> FloatVal (x+y)
                                                             (IntVal x, FloatVal y)   -> FloatVal (fromIntegral(x) + y)
                                                             (FloatVal x, IntVal y)   -> FloatVal (x + fromIntegral(y))
                                                             _                        -> StrVal "Type Error"
                                           do
                                             case (result1, result2) of
                                               (Just (IntVal x), IntVal y) -> (x==y)
                                               (Just (FloatVal x), FloatVal y) -> (x==y)
                                               (Just (StrVal x), StrVal y) -> (x==y)

-- Tests basic subtraction between two atomic values
prop_simpleSubtract :: Value -> Value -> Bool
prop_simpleSubtract a b = do
                            case (a,b) of
                              (VarVal i, _) -> True --ignore var val since we aren't dealing with Lstate
                              (_, VarVal i) -> True --ignore var val since we aren't dealing with Lstate
                              _             -> do
                                                let result1 = eval emptytree (Subtract (Val a) (Val b))
                                                let result2 = case (a,b) of
                                                                 (IntVal x, IntVal y)     -> IntVal (x-y)
                                                                 (FloatVal x, FloatVal y) -> FloatVal (x-y)
                                                                 (IntVal x, FloatVal y)   -> FloatVal (fromIntegral(x) - y)
                                                                 (FloatVal x, IntVal y)   -> FloatVal (x - fromIntegral(y))
                                                                 _                        -> StrVal "Type Error"
                                                do
                                                 case (result1, result2) of
                                                   (Just (IntVal x), IntVal y) -> (x==y)
                                                   (Just (FloatVal x), FloatVal y) -> (x==y)
                                                   (Just (StrVal x), StrVal y) -> (x==y)

-- tests basic multiplication between two atomic values
prop_simpleMultiply :: Value -> Value -> Bool
prop_simpleMultiply a b = do
                            case (a,b) of
                              (VarVal i, _) -> True --ignore var val since we aren't dealing with Lstate
                              (_, VarVal i) -> True --ignore var val since we aren't dealing with Lstate
                              _             -> do
                                                let result1 = eval emptytree (Mul (Val a) (Val b))
                                                let result2 = case (a,b) of
                                                                 (IntVal x, IntVal y)     -> IntVal (x*y)
                                                                 (FloatVal x, FloatVal y) -> FloatVal (x*y)
                                                                 (IntVal x, FloatVal y)   -> FloatVal (fromIntegral(x) * y)
                                                                 (FloatVal x, IntVal y)   -> FloatVal (x * fromIntegral(y))
                                                                 _                        -> StrVal "Type Error"
                                                do
                                                 case (result1, result2) of
                                                   (Just (IntVal x), IntVal y) -> (x==y)
                                                   (Just (FloatVal x), FloatVal y) -> (x==y)
                                                   (Just (StrVal x), StrVal y) -> (x==y)

-- tests basic division between two atomic values
prop_simpleDivide :: Value -> Value -> Bool
prop_simpleDivide a b = do
                          case (a,b) of
                              (VarVal i, _) -> True --ignore var val since we aren't dealing with Lstate
                              (_, VarVal i) -> True --ignore var val since we aren't dealing with Lstate
                              _             -> do
                                                let result1 = eval emptytree (Div (Val a) (Val b))
                                                let result2 = case (a,b) of
                                                                 (IntVal x, IntVal 0)            -> StrVal "Div by 0 error"
                                                                 (FloatVal x, IntVal 0)          -> StrVal "Div by 0 error"
                                                                 (IntVal x, FloatVal 0.0)        -> StrVal "Div by 0 error"
                                                                 (FloatVal x, FloatVal 0.0)      -> StrVal "Div by 0 error"
                                                                 (IntVal x, IntVal y)            -> IntVal (x`div`y)
                                                                 (FloatVal x, FloatVal y)        -> FloatVal (x/y)
                                                                 (IntVal x, FloatVal y)          -> FloatVal (fromIntegral(x) / y)
                                                                 (FloatVal x, IntVal y)          -> FloatVal (x / fromIntegral(y))
                                                                 _                               -> StrVal "Type Error"
                                                do
                                                 case (result1, result2) of
                                                   (Just (IntVal x), IntVal y) -> (x==y)
                                                   (Just (FloatVal x), FloatVal y) -> (x==y)
                                                   (Just (StrVal x), StrVal y) -> (x==y)

-- tests modulus operator on two atomic values
prop_simpleMod :: Value -> Value -> Bool
prop_simpleMod a b = do
                          case (a,b) of
                              (VarVal i, _) -> True --ignore var val since we aren't dealing with Lstate
                              (_, VarVal i) -> True --ignore var val since we aren't dealing with Lstate
                              _             -> do
                                                let result1 = eval emptytree (Mod (Val a) (Val b))
                                                let result2 = case (a,b) of
                                                                 (IntVal x, IntVal 0)            -> StrVal "Div by 0 error"
                                                                 (IntVal x, IntVal y)            -> IntVal (x`mod`y)
                                                                 _                               -> StrVal "Type Error"
                                                do
                                                 case (result1, result2) of
                                                   (Just (IntVal x), IntVal y)     -> (x==y)
                                                   (Just (FloatVal x), FloatVal y) -> (x==y)
                                                   (Just (StrVal x), StrVal y)     -> (x==y)



-- tests area of a circle function given two atomic values
prop_simplecirA :: Value -> Bool
prop_simplecirA a = do
                      case (a) of
                        (VarVal i) -> True --ignore var val since we aren't dealing with Lstate
                        _          -> do
                                       let result1 = eval emptytree (CirA (Val a))
                                       let result2 = case (a) of
                                                         (IntVal x)     -> FloatVal (fromIntegral(x)^2*pi)
                                                         (FloatVal x)   -> FloatVal (x*x*pi)
                                                         _              -> StrVal "Type Error"
                                       do
                                                 case (result1, result2) of
                                                   (Just (IntVal x), IntVal y) -> (x==y)
                                                   (Just (FloatVal x), FloatVal y) -> (x==y)
                                                   (Just (StrVal x), StrVal y) -> (x==y)

-- run the tests
main :: IO ()
main = do
         quickCheck prop_simpleAdd
         quickCheck prop_simpleSubtract
         quickCheck prop_simpleMultiply
         quickCheck prop_simpleDivide
         quickCheck prop_simpleMod
         quickCheck prop_simplecirA
