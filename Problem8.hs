import Test.QuickCheck
import Test.QuickCheck.All

{-
    (**) Eliminate consecutive duplicates of list elements.

    If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

    Example:

    * (compress '(a a a a b c c a a d e e e e))
    (A B C A D E)
    Example in Haskell:

    > compress "aaaabccaadeeee"
    "abcade"
-}

{- ------------- -}
{- SOLUTION HERE -}
{- ------------- -}

{- ! This solutin remove all duplicated itens and is not the target solution!!!

compress :: (Eq a) => [a] -> [a]
compress xs = verify xs []
    where
        verify :: (Eq a) => [a] -> [a] -> [a]
        verify [] acc = acc
        verify (x:xs) acc =
            if not (x `elem` acc) then
                 verify xs (x:acc)
            else
                verify xs acc
-}

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs) = if x == head xs then
                    compress xs
                else
                    x:compress xs

{- ------------- -}
{- TEST CASE     -}
{- ------------- -}

main = quickCheck (compress "aaaabccaadeeee" == "abcade")
