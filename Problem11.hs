import Test.QuickCheck
import Test.QuickCheck.All

{-
    (*) Modified run-length encoding.

    Modify the result of problem 10 in such a way that if an element has no duplicates
    it is simply copied into the result list. Only elements with duplicates are
    transferred as (N E) lists.

    Example:

    * (encode-modified '(a a a a b c c a a d e e e e))
    ((4 A) B (2 C) (2 A) D (4 E))
    Example in Haskell:

    P11> encodeModified "aaaabccaadeeee"
    [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

{- ------------- -}
{- SOLUTION HERE -}
{- ------------- -}

data Encoding = Multiple Int Char
              | Single Char
              deriving (Show, Eq)

encodeModified :: [Char] -> [Encoding]
encodeModified [] = []
encodeModified xs = do
    let result = takeWhile (\x -> x == head xs) xs
    let (_, second) = splitAt (length result) xs
    [encode result] ++ (encodeModified second)
    where
        encode :: [Char] -> Encoding
        encode xs =
            if length xs == 1 then
                Single (head xs)
            else
                Multiple (length xs) (head xs)

{- ------------- -}
{- TEST CASE     -}
{- ------------- -}

main = quickCheck (encodeModified "aaaabccaadeeee" == [Multiple 4 'a',
                                                        Single 'b',
                                                        Multiple 2 'c',
                                                        Multiple 2 'a',
                                                        Single 'd',
                                                        Multiple 4 'e'])
