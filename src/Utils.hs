module Utils (allTrue, joinDelim, boundWrapAround) where

allTrue :: [Bool] -> Bool
allTrue = all (== True)

joinDelim :: String -> [String] -> String
joinDelim d = foldr (\a b -> a ++ d ++ b) ""

boundWrapAround :: (Int -> Int) -> Int -> Int -> Int -> Int
boundWrapAround f l u i
  | o < l = u
  | o > u = l
  | otherwise = o
  where
    o = f i
