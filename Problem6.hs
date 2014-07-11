import Test.QuickCheck
import Test.QuickCheck.All

{-
    (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

    Example in Haskell:

    *Main> isPalindrome [1,2,3]
    False
    *Main> isPalindrome "madamimadam"
    True
    *Main> isPalindrome [1,2,4,8,16,8,4,2,1]
    True
-}

{- ------------- -}
{- SOLUTION HERE -}
{- ------------- -}

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = if (length xs) `mod` 2 /= 0 then
                      False
                  else
                        if x == (last xs) then
                              isPalindrome (init xs)
                        else
                              False

isPalindrome' xs = xs == (reverse xs)

isPalindrome'' [] = True
isPalindrome'' [_] = True
isPalindrome'' (x:xs) = x == last xs && isPalindrome'' (init xs)

{- ------------- -}
{- TEST CASE     -}
{- ------------- -}

main = do
    quickCheck (isPalindrome [1,2,3] == False)
    quickCheck (isPalindrome "madamimadam" == True)
    quickCheck (isPalindrome [1,2,4,8,16,8,4,2,1] == True)
    quickCheck (isPalindrome' [1,2,3] == False)
    quickCheck (isPalindrome' "madamimadam" == True)
    quickCheck (isPalindrome' [1,2,4,8,16,8,4,2,1] == True)
    quickCheck (isPalindrome'' [1,2,3] == False)
    quickCheck (isPalindrome'' "madamimadam" == True)
    quickCheck (isPalindrome'' [1,2,4,8,16,8,4,2,1] == True)
