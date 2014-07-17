import Test.QuickCheck
import Test.QuickCheck.All

{-
    (**) Replicate the elements of a list a given number of times.

    Example:

    * (repli '(a b c) 3)
    (A A A B B B C C C)
    Example in Haskell:

    > repli "abc" 3
    "aaabbbccc"
-}

{- ------------- -}
{- SOLUTION HERE -}
{- ------------- -}

repli :: [a] -> Int -> [a]
repli [] _ = []
repli _  0 = []
repli xs 1 = xs
repli (x:xs) c = (take c (repeat x)) ++ repli xs c

{- ------------- -}
{- TEST CASE     -}
{- ------------- -}

main = quickCheck (repli "abc" 3 == "aaabbbccc")
