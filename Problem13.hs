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

encodeDirect :: [Char] -> [Encoding]
encodeDirect [] = []
encodeDirect str@(c:cs) = if (length list) > 1 then
                        [Multiple (length list) c] ++ encodeDirect (drop (length list - 1) cs)
                      else
                        [Single c] ++ encodeDirect (drop (length list - 1) cs)
                    where list = (takeWhile (\x -> x == c) str)


{- ------------- -}
{- TEST CASE     -}
{- ------------- -}

main = quickCheck (encodeDirect "aaaabccaadeeee" == [Multiple 4 'a',
                                                     Single 'b',
                                                     Multiple 2 'c',
                                                     Multiple 2 'a',
                                                     Single 'd',
                                                     Multiple 4 'e'])
