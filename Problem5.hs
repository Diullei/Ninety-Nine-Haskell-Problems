import Test.QuickCheck
import Test.QuickCheck.All

{-
	(*) Reverse a list.

	Example in Haskell:

	Prelude> myReverse "A man, a plan, a canal, panama!"
	"!amanap ,lanac a ,nalp a ,nam A"
	Prelude> myReverse [1,2,3,4]
	[4,3,2,1]
-}

{- ------------- -}
{- SOLUTION HERE -}
{- ------------- -}

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

{- ------------- -}
{- TEST CASE     -}
{- ------------- -}

main = do
	quickCheck (myReverse "A man, a plan, a canal, panama!" == "!amanap ,lanac a ,nalp a ,nam A")
	quickCheck (myReverse [1,2,3,4] == [4,3,2,1])
