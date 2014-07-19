import           Test.QuickCheck
import           Test.QuickCheck.All

{-
    Insert an element at a given position into a list.

    Example:

    * (insert-at 'alfa '(a b c d) 2)
    (A ALFA B C D)
    Example in Haskell:

    P21> insertAt 'X' "abcd" 2
    "aXbcd"
-}

{- ------------- -}
{- SOLUTION HERE -}
{- ------------- -}

insertAt :: a -> [a] -> Int -> [a]
insertAt elem xs p =
    (take (p-1) xs) ++ [elem] ++ (drop (p-1) xs)

{- ------------- -}
{- TEST CASE     -}
{- ------------- -}

main = quickCheck (insertAt 'X' "abcd" 2 == "aXbcd")
