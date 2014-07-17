import Test.QuickCheck
import Test.QuickCheck.All

{-
    (**) Extract a slice from a list.

    Given two indices, i and k, the slice is the list containing the elements between
    the i'th and k'th element of the original list (both limits included). Start
    counting the elements with 1.

    Example:

    * (slice '(a b c d e f g h i k) 3 7)
    (C D E F G)
    Example in Haskell:

    *Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
    "cdefg"
-}

{- ------------- -}
{- SOLUTION HERE -}
{- ------------- -}

slice :: [a] -> Int -> Int -> [a]
slice xs start end =
    takeWhile' xs start end 1
    where
        takeWhile' :: [a] -> Int -> Int -> Int -> [a]
        takeWhile' []     _     _   _     = []
        takeWhile' (x:xs) start end index =
            if index >= start && index <= end then
                x:takeWhile' xs start end (index + 1)
            else
                takeWhile' xs start end (index + 1)

slice' :: [a] -> Int -> Int -> [a]
slice' xs start end = take (end - start + 1) $ drop (start -1) xs

{- ------------- -}
{- TEST CASE     -}
{- ------------- -}

main = do
    quickCheck (slice ['a','b','c','d','e','f','g','h','i','k'] 3 7 == "cdefg")
    quickCheck (slice' ['a','b','c','d','e','f','g','h','i','k'] 3 7 == "cdefg")
