main :: IO ()
main = print $ search' 2014


type Triangle = [Row]
type Row = [Bool]

-- >>> search' 1
-- 3
-- >>> search' 2
-- 5
-- >>> search' 3
-- 5
-- >>> search' 4
-- 5
search' :: Int -> Int
search' n = search 1 n triangle

search :: Int -- Current row number
       -> Int -- How many Falses left?
       -> Triangle -- Left rows bellow
       -> Int -- The answer
search current n (r:rs)
  | falses <= 0 = current
  | otherwise = search (current + 1) falses rs
 where
  falses = n - countFalses r

countFalses :: Row -> Int
countFalses = length . filter not

-- >>> take 4 triangle
-- [[True],[True,True],[True,False,True],[True,True,True,True]]
triangle :: Triangle
triangle = iterate nextRow [True]

-- >>> nextRow [True]
-- [True,True]
-- >>> nextRow $ nextRow [True]
-- [True,False,True]
-- >>> nextRow $ nextRow $ nextRow [True]
-- [True,True,True,True]
nextRow :: Row -> Row
nextRow row = (True:) . snd . foldr (\x (past, ls) -> (x, x `xor` past : ls)) (True, [True]) $ init row

xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False


{- ==================
 -  Helper Functions
 - ================== -}

{-| The 'formatTriangle' formats and show given triangle.
 -}
formatTriangle :: Int -> Triangle -> IO ()
formatTriangle n = putStrLn . unlines . map (unwords . map showBool) . take n
 where
  showBool True = "1"
  showBool False = "0"
