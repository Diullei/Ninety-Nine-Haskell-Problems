import Test.QuickCheck
import Test.QuickCheck.All

{-
    (**) Drop every N'th element from a list.

    Example:

    * (drop '(a b c d e f g h i k) 3)
    (A B D E G H K)
    Example in Haskell:

    *Main> dropEvery "abcdefghik" 3
    "abdeghk"
-}

{- ------------- -}
{- SOLUTION HERE -}
{- ------------- -}

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs p = get 1 xs
    where
        get :: Int -> [a] -> [a]
        get _ [] = []
        get i (x:xs) =
            if (i `mod` p) /= 0 then
                x:get (i + 1) xs
            else
                get (i + 1) xs

{- ------------- -}
{- TEST CASE     -}
{- ------------- -}

main = quickCheck (dropEvery "abcdefghik" 3 == "abdeghk")
