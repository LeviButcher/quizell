module Utils where

import Data.Maybe (fromMaybe)
import Data.Time (DiffTime, NominalDiffTime, defaultTimeLocale, formatTime)
import System.Console.ANSI (Color (Blue, Cyan, Green, Magenta, Red), ColorIntensity (Dull, Vivid), ConsoleLayer (Foreground), SGR (Reset, SetColor), clearScreen, setCursorPosition, setSGR)
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
  toLog :: (String -> IO String) -> a -> IO ()
  readLog :: (String -> IO String) -> IO [a]

numberStrings :: [String] -> String
numberStrings s = unlines $ (\(a, b) -> concat [show a, ") ", b]) <$> zip [1 ..] s

getNumberOrDefault :: Int -> IO Int
getNumberOrDefault d = fromMaybe d . readMaybe <$> getLine

resetScreen :: IO ()
resetScreen = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

prettyText :: [SGR] -> (String -> IO ()) -> String -> IO ()
prettyText conf f x = do
  setSGR conf
  f x
  setSGR [Reset]
  return ()

errorText :: (String -> IO ()) -> String -> IO ()
errorText = prettyText [SetColor Foreground Vivid Red]

infoText :: (String -> IO ()) -> String -> IO ()
infoText = prettyText [SetColor Foreground Vivid Blue]

goodNewsText :: (String -> IO ()) -> String -> IO ()
goodNewsText = prettyText [SetColor Foreground Vivid Cyan]