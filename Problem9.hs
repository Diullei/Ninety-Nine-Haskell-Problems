import Test.QuickCheck
import Test.QuickCheck.All

{-
    (**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

    Example:

    * (pack '(a a a a b c c a a d e e e e))
    ((A A A A) (B) (C C) (A A) (D) (E E E E))
    Example in Haskell:

    *Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a',
                 'a', 'd', 'e', 'e', 'e', 'e']
    ["aaaa","b","cc","aa","d","eeee"]
-}

{- ------------- -}
{- SOLUTION HERE -}
{- ------------- -}

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs = do
    let result = takeWhile (\x -> x == head xs) xs
    let (_, second) = splitAt (length result) xs
    [result] ++ (pack second)

{- ------------- -}
{- TEST CASE     -}
{- ------------- -}

main = quickCheck (pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] == ["aaaa","b","cc","aa","d","eeee"])
