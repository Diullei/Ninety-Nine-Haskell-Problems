import Test.QuickCheck
import Test.QuickCheck.All

{-
    (*) Split a list into two parts; the length of the first part is given.

    Do not use any predefined predicates.

    Example:

    * (split '(a b c d e f g h i k) 3)
    ( (A B C) (D E F G H I K))
    Example in Haskell:

    *Main> split "abcdefghik" 3
    ("abc", "defghik")
-}

{- ------------- -}
{- SOLUTION HERE -}
{- ------------- -}

split :: [a] -> Int -> ([a], [a])
split xs i = (partL xs 1, partR xs 1)
    where
        partL :: [a] -> Int -> [a]
        partL (x:xs) p =
            if p <= i then
                x:partL xs (p + 1)
            else
                []
        partR :: [a] -> Int -> [a]
        partR []     _ = []
        partR (x:xs) p =
            if p > i then
                x:partR xs (p + 1)
            else
                partR xs (p + 1)


{- ------------- -}
{- TEST CASE     -}
{- ------------- -}

main = quickCheck (split "abcdefghik" 3 == ("abc", "defghik"))
