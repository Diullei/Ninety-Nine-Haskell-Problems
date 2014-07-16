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

myLast' :: [a] -> a
myLast' = foldr1 (flip const)

-- why this is not working?
--myLast'' :: [a] -> a
--myLast'' = read . reverse

{- ------------- -}
{- TEST CASE     -}
{- ------------- -}

main = do
    quickCheck (myLast [1,2,3,4] == 4)
    quickCheck (myLast' [1,2,3,4] == 4)
    --quickCheck (myLast'' [1,2,3,4] == 4)
