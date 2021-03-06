module Mainx where

import MainHelpers
  ( ProgramArgs (..),
    normalApp,
    runQuizell,
  )
import qualified QuizResults as QR
import System.Environment (getArgs, getEnv)
import System.Exit (exitSuccess)
import Text.Printf (printf)
import Utils (Log (toLog), getNumberOrDefault, numberStrings, resetScreen)

setQuizFile :: IO String
setQuizFile = do
  resetScreen
  putStrLn "Enter Path to quiz file:"
  getLine

setQuizLength :: IO Int
setQuizLength = do
  resetScreen
  putStrLn "Enter amount of questions to ask:"
  getNumberOrDefault 0

setQuizTimer :: IO Int
setQuizTimer = do
  resetScreen
  putStrLn "Enter Amount of time for quiz in seconds (0 is infinity):"
  getNumberOrDefault 0

prettyConfig :: ProgramArgs -> String
prettyConfig (ProgramArgs file qLength _ showRes time) =
  printf "file=\"%s\"    questionCount=%d    time=%d   showYourPastScore=%s" file qLength time (show showRes)

loop :: a -> (a -> IO a) -> (a -> IO ()) -> IO a
loop args setupArgs runArgs = do
  updatedArgs <- setupArgs args
  runArgs updatedArgs
  loop updatedArgs setupArgs runArgs

-- Would love to somehow have the menu option be hard typed to avoid runtime errors
main :: IO ()
main = do
  args <- getDefaultQuizellArgs
  loop args setup (\x -> runQuizell x QR.getWindowsStorage getWindowsUserName normalApp)
  return ()
  where
    setup args = do
      resetScreen
      putStrLn "QUIZELL - WINDOWS\n"
      putStrLn $ "Config: " ++ prettyConfig args
      putStrLn ""
      putStrLn $ numberStrings ["Set Quiz File Path", "Set Quiz Length", "Set Timer", "Show My Past Results", "Run Quiz", "Quit"]
      putStrLn "Enter Menu Option Number:"
      ent <- getNumberOrDefault (-1)
      case ent of
        1 -> do
          file <- setQuizFile
          setup $ args {quizPath = file}
        2 -> do
          qLength <- setQuizLength
          setup $ args {quizLength = qLength}
        3 -> do
          qTime <- setQuizTimer
          setup $ args {time = qTime}
        4 -> do
          return $ args {showResults = True}
        5 -> return args
        6 -> putStrLn "Thank you for using quizell!" >> exitSuccess
        _ -> putStrLn "Invalid Menu Option (Enter to Cont.)" >> getLine >> setup args

getWindowsUserName :: IO String
getWindowsUserName = getEnv "USERNAME"

getDefaultQuizellArgs :: IO ProgramArgs
getDefaultQuizellArgs =
  return
    ProgramArgs
      { quizPath = "",
        quizLength = 0,
        showResults = False,
        time = 0,
        tuiOn = False
      }