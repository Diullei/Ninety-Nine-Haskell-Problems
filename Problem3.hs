import Test.QuickCheck
import Test.QuickCheck.All

{-
    (*) Find the K'th element of a list. The first element in the list is number 1.

    Example:

    * (element-at '(a b c d e) 3)
    c
-}

{- ------------- -}
{- SOLUTION HERE -}
{- ------------- -}

elementAt :: [a] -> Int -> a
elementAt [] _ = error "Empty list"
elementAt xs pos = xs !! (pos - 1)

{- ------------- -}
{- TEST CASE     -}
{- ------------- -}

main = do
    quickCheck (elementAt ['a', 'b', 'c', 'd', 'e'] 3 == 'c')
