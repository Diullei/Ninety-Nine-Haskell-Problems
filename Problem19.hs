import           Test.QuickCheck
import           Test.QuickCheck.All

{-
    (**) Rotate a list N places to the left.

    Hint: Use the predefined functions length and (++).

    Examples:

    * (rotate '(a b c d e f g h) 3)
    (D E F G H A B C)

    * (rotate '(a b c d e f g h) -2)
    (G H A B C D E F)
    Examples in Haskell:

    *Main> rotate ['a','b','c','d','e','f','g','h'] 3
    "defghabc"

    *Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
    "ghabcdef"
-}

{- ------------- -}
{- SOLUTION HERE -}
{- ------------- -}

rotate :: [a] -> Int -> [a]
rotate xs i = take (length xs) $
    cycle ((
        drop (
            if i > 0 then
                i
            else
                (length xs) + i
            )
        xs)
    ++ xs)

{- ------------- -}
{- TEST CASE     -}
{- ------------- -}

main = do
    quickCheck (rotate ['a','b','c','d','e','f','g','h'] 3 == "defghabc")
    quickCheck (rotate ['a','b','c','d','e','f','g','h'] (-2) == "ghabcdef")
