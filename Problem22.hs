import           Test.QuickCheck
import           Test.QuickCheck.All

{-
    Create a list containing all integers within a given range.

    Example:

    * (range 4 9)
    (4 5 6 7 8 9)
    Example in Haskell:

    Prelude> range 4 9
    [4,5,6,7,8,9]
-}

{- ------------- -}
{- SOLUTION HERE -}
{- ------------- -}

range :: Int -> Int -> [Int]
range start end | end <= start = [0]
                | otherwise = take (end - start + 1) [start..]

{- ------------- -}
{- TEST CASE     -}
{- ------------- -}

main = quickCheck (range 4 9 == [4,5,6,7,8,9])
