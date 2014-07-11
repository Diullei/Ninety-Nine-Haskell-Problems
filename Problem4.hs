import Test.QuickCheck
import Test.QuickCheck.All

{-
    (*) Find the number of elements of a list.

    Example in Haskell:

    Prelude> myLength [123, 456, 789]
    3
    Prelude> myLength "Hello, world!"
    13
-}

{- ------------- -}
{- SOLUTION HERE -}
{- ------------- -}

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)

{- ------------- -}
{- TEST CASE     -}
{- ------------- -}

main = do
    quickCheck (myLength [123, 456, 789] == 3)
    quickCheck (myLength "Hello, world!" == 13)
