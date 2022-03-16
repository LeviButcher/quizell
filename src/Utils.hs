module Utils where

import Data.Maybe (fromMaybe)
import Data.Time (DiffTime, NominalDiffTime, defaultTimeLocale, formatTime)
import Text.Read (readMaybe)

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

getTimeString :: NominalDiffTime -> String
getTimeString = formatTime defaultTimeLocale "%hh:%mm:%ss"

class Log a where
  toLog :: a -> IO ()
  readLog :: IO [a]

numberStrings :: [String] -> String
numberStrings s = unlines $ (\(a, b) -> concat [show a, ") ", b]) <$> zip [1 ..] s

getNumberOrDefault :: Int -> IO Int
getNumberOrDefault d = fromMaybe d . readMaybe <$> getLine