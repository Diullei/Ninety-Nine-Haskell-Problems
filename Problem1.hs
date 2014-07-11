import Test.QuickCheck
import Test.QuickCheck.All

{-
    (*) Find the last element of a list.

    (Note that the Lisp transcription of this problem is incorrect.)

    Example in Haskell:

    Prelude> myLast [1,2,3,4]
    4
    Prelude> myLast ['x','y','z']
    'z'
-}

{- ------------- -}
{- SOLUTION HERE -}
{- ------------- -}

myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

{- ------------- -}
{- TEST CASE     -}
{- ------------- -}

myLast_test xs = myLast xs == last xs

{- ------------- -}
{- EXECUTE TESTS -}
{- ------------- -}

main = do
    quickCheck (myLast_test [1,2,3,4])
