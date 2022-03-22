import Test.QuickCheck
import Expr
import REPL

--Tests on num calc (including "+","-","*",)
--"/","^","abs","mod"

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

{-
--test for Float & Int multiplication
prop_multiFloatInt :: Float -> Int -> Bool
prop_multiFloatInt a b = (a*b) == (b*a)
-}

--test for Int divided by Int(divisor not 0)
--prop_divInt :: Int -> Float -> Bool
--prop_divInt a b =
--    not(b==0) ==>
--     a/b == a*(1/b)

{-
--test for Int divided by Float(divisor not 0)
prop_divIntFloat :: Int -> Float -> Float -> Bool
prop_divIntFloat a b = a/b == b*(1/a)

--test for Float divided by Int(divisor not 0)
prop_divFloatInt :: Float -> Int -> Float -> Bool
prop_divFloatInt a b =
--    not(b==0) ==>
--     a/b == a*(1/b)
-}

--test for Float divided by Float(divisor not 0)
--prop_divFloat :: Float -> Float -> Bool
--prop_divFloat a b = a/b == a*(1/b)

{-
--test for division when divisor equals 0
prop_
-}

{-
--test for Int power Calculation 
prop_powInt :: Int ->Int ->Bool
pro_powInt a b = (a ^ b) == 
-}

main :: IO ()
main = do
    putStrLn "Running Tests..."
    quickCheck (verbose prop_addedInt) 
    quickCheckWith stdArgs { maxSuccess = 1000 }prop_addedFloat
    quickCheck prop_subInt
    quickCheck prop_subFloat
    quickCheck prop_multiInt
    quickCheck prop_multiFloat
    --quickCheck prop_multiFloatInt
    --quickCheck (verbose prop_divInt)
    --quickCheck prop_divIntFloat    
    --quickCheck (verbose prop_divFloatInt)
    --quickCheck prop_divFloat
    putStrLn "Done!"
