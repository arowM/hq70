import Data.List (nub, sort, union)

main :: IO ()
main = print . length $ nthConnector 20

{-| Data representing connections of connectors
 -}
data Con = NoCon
         | Con2 Con Con
         | Con3 Con Con Con
  deriving (Show, Eq, Ord)

{-|
 - >>> nthConnector 2
 - [Con2 NoCon NoCon]
 -}
nthConnector :: Int -> [Con]
nthConnector n = connectors !! (n - 2)

{-| Main function to find n-sockets connectors.
 -  Make sure that all element list are sorted.
 -
 -  prop> map sort connectors !! n == connectors !! n
 -}
connectors :: [[Con]]
connectors = [ Con2 NoCon NoCon ]       -- two sockets
           : [ Con2 NoCon emptyCon2
             , Con3 NoCon NoCon NoCon
             ]                          -- three sockets
           : zipWith unionCon connectors (tail connectors)

emptyCon2 :: Con
emptyCon2 = Con2 NoCon NoCon

emptyCon3 :: Con
emptyCon3 = Con3 NoCon NoCon NoCon

unionCon :: [Con] -> [Con] -> [Con]
unionCon cs1 cs2 =
  union (addToCons emptyCon3 cs1) (addToCons emptyCon2 cs2)

{-| Plugs a connections to candidate connections list.
 - Assumes that second argument is already sorted.
 -}
addToCons :: Con -> [Con] -> [Con]
addToCons c cs = nub $ concatMap (addToCon c) cs

{-| Make sure all connections are normalized.
 -}
addToCon :: Con -> Con -> [Con]
addToCon c NoCon = [c]
addToCon x (Con2 a b) = union (map (\a' -> normalize $ Con2 a' b) (addToCon x a))
                                     (map (\b' -> normalize $ Con2 a b') (addToCon x b))
addToCon x (Con3 a b c) = (map (\a' -> normalize $ Con3 a' b c) (addToCon x a))
                  `union` (map (\b' -> normalize $ Con3 a b' c) (addToCon x b))
                  `union` (map (\c' -> normalize $ Con3 a b c') (addToCon x c))

normalize :: Con -> Con
normalize NoCon = NoCon
normalize x@(Con2 a b)
  | a > b = Con2 b a
  | otherwise = x
normalize (Con3 a b c) = Con3 a' b' c'
 where
  [a', b', c'] = sort [a, b, c]
