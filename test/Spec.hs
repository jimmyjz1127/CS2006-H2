import Expr
import REPL
import BinaryTree
import Test.QuickCheck
--import GHC.IO.Encoding
--import System.Random
--import GHC.Generics ( Generic )
--import Generic.Random.Generic

--Tests on num calculation (including "+","-","*","/","^","abs","mod","pow")

--test for Int addition 
prop_addedInt :: Int -> Int -> Bool
prop_addedInt a b = (a+b) == (b+a)

-- test for Float addition
prop_addedFloat :: Float -> Float-> Bool
prop_addedFloat a b = (a+b) == (b+a)

--test for Int Subtraction
prop_subInt :: Int-> Int ->Bool
prop_subInt a b = (a-b) == -(b-a)

--test for Float Subtraction
prop_subFloat :: Float-> Float ->Bool
prop_subFloat a b = (a-b) == -(b-a)

--test for Int multiplication
prop_multiInt :: Int -> Int -> Bool
prop_multiInt a b = (a*b) == (b*a)

--test for Float multiplication
prop_multiFloat :: Float -> Float -> Bool
prop_multiFloat a b = (a*b) == (b*a)

--test for Float & Int multiplication
--prop_multiFloatInt :: Float -> Int -> Bool
--prop_multiFloatInt a b = (a*b) == (b*a)

--test for Int divided by Int(divisor int not 0)
--prop_divInt :: Int -> Int -> Bool
--prop_divInt a b = a/b == a* (1/b)
--    not(b==0) ==>

--test for Int divided by Float 
--prop_divIntFloat :: Int -> Float -> Bool
--prop_divIntFloat a b = a/b == a* (1/b)

--test for Float divided by Int(divisor int not 0)
--prop_divFloatInt :: Float -> Int -> Bool
--prop_divFloatInt a b = a/b == a* (1/b) 
--    not(b==0) ==>
    
--test for Float divided by Float
prop_divFloat :: Float -> Float -> Bool
prop_divFloat a b = a/b == a*(1/b)

--test for division when divisor equals 0
--prop_divByZero :: Int -> Int -> Bool
--prop_divByZero a b 
--    (b==0) ==>
--    a/b == "Div by 0 error"

{-
--test for Int power Calculation 
prop_powInt :: Int -> Int -> Bool
prop_powInt a b = (a ^ b) /a == a ^ (b-1) 
-}

--test for Int mod Calculation
--prop_mod :: Int -> Int -> Bool
--prop_mod a b = 
--    not(b==0) ==>
--    (a % b) == (a - a/b)

--test for mod calculation when the divisor equals 0
--prop_modByZero :: Int -> Int -> Bool
--prop_modByZero a b
--    (b==0) ==>
--    (a % b) == "Div by 0 error"
{-
--test for positive number abs calculation
prop_absPos :: Int -> Int -> Bool
prop_absPos a = abs(a) == a

--test for negative number abs calculation
prop_absNeg :: Int -> Int -> Bool
prop_absNeg a = abs(a) == -a

--test for exception(Type Dismatch, i.e. expected to print out "Type Error") 
--e.g. adding String to Int
prop_typeDismatch :: Int -> String -> Bool
prop_typeDismatch a b = a + b == "Type Error"

--tests on type conversion

--test for int to float
prop_swapInttoFloat :: Int -> Bool
prop_swapInttoFloat a = a == swap(swap(a))

--test for float to int
prop_swapFloattoInt :: Float -> Bool
prop_swapFloattoInt a = a == swap(swap(a))

--test for int to String
prop_toString :: Int -> String -> Bool
prop_toString a = a == toInt(toString(a))

--test for String to int
prop_toInt :: String -> Int -> Bool
prop_toInt a = a == toString(toInt(a))

--test for String to Float
prop_toFloat :: String -> Float -> Bool
prop_toFloat a = a == toString(toFloat(a))
-}

--tests on a new generator using arbitrary, to have different default types(including char, int, expr)
--to allow different inputs

--inputting random types
{-
class Arbitrary a where
  arbitrary :: Gen a 

data MyType = MyType {
    b :: Int
  , c :: Float
  , d :: Char
  , e :: String
  } deriving (Show, Generic)

generate (genericArbitrary :: Gen MyType) -- QuickCheck uses them to build things in this form

prop_Add :: Gen Result
prop_Add = do
  (x, y) <- arbitrary :: Gen (Int, Int)
  return $ if x + y == y + x
    then succeeded
    else failed { reason = "failed addition" }
-}

--    data Tree a = Leaf | Node (Tree a) a (Tree a)
--     deriving (Show, Generic)

--    instance Arbitrary a => Arbitrary (Tree a) where
--      arbitrary = genericArbitrary' Z
    
--    prop_sample =  sample (arbitrary :: Gen (Tree ()))

main :: IO ()
main = do
--    hSetEncoding stdout utf8
--    hSetEncoding stderr utf8
--    setLocaleEncoding utf8
    putStrLn "Running Tests..."
    quickCheck (verbose prop_addedInt) 
    quickCheckWith stdArgs { maxSuccess = 1000 }prop_addedFloat
    quickCheck prop_subInt
    quickCheck prop_subFloat
    quickCheck prop_multiInt
    quickCheck prop_multiFloat
--    quickCheck prop_multiFloatInt
--    quickCheck prop_divInt
--    quickCheck prop_divIntFloat    
--    quickCheck prop_divFloatInt
--    quickCheck prop_divFloat
--    quickCheck prop_divByZero
--    quickCheck prop_powInt 
--    quickCheck prop_mod
--    quickCheck prop_modByZero
--    quickCheck prop_absPos
--    quickCheck prop_absNeg
--    quickCheck prop_typeDismatch
--    quickCheck prop_swapInttoFloat
--    quickCheck prop_swapFloattoInt
--    quickCheck prop_toString
--    quickCheck prop_toInt
--    quickCheck prop_toFloat
--    quickCheck prop_add
    putStrLn "Done!"
