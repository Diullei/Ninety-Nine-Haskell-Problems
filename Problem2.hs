import Test.QuickCheck
import Test.QuickCheck.All

{-
    (*) Find the last but one element of a list.

    (Note that the Lisp transcription of this problem is incorrect.)

    Example in Haskell:

    Prelude> myButLast [1,2,3,4]
    3
    Prelude> myButLast ['a'..'z']
    'y'
-}

{- ------------- -}
{- SOLUTION HERE -}
{- ------------- -}

myButLast :: [a] -> a
myButLast xs = last $ init xs

{- ------------- -}
{- TEST CASE     -}
{- ------------- -}

main = do
    quickCheck (myButLast [1,2,3,4] == 3)
    quickCheck (myButLast ['a'..'z'] == 'y')
