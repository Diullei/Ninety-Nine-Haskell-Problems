import Test.QuickCheck
import Test.QuickCheck.All

{-
    (**) Decode a run-length encoded list.

    Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

    Example in Haskell:

    P12> decodeModified
           [Multiple 4 'a',Single 'b',Multiple 2 'c',
            Multiple 2 'a',Single 'd',Multiple 4 'e']
    "aaaabccaadeeee"
-}

{- ------------- -}
{- SOLUTION HERE -}
{- ------------- -}

data Encoding = Multiple Int Char
              | Single Char
              deriving (Show, Eq)

decodeModified :: [Encoding] -> [Char]
decodeModified [] = []
decodeModified (x:xs)  =
    case x of
        Multiple n c -> (take n (repeat c)) ++ decodeModified xs
        Single   c   -> [c]                 ++ decodeModified xs

{- ------------- -}
{- TEST CASE     -}
{- ------------- -}

main = quickCheck (decodeModified [Multiple 4 'a',
                                   Single 'b',
                                   Multiple 2 'c',
                                   Multiple 2 'a',
                                   Single 'd',
                                   Multiple 4 'e'] == "aaaabccaadeeee")
