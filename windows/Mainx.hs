module Mainx where

import Control.Applicative ((<**>))
import Control.Arrow (ArrowChoice (left))
import Control.Exception (try)
import Control.Exception.Base (Exception)
import Control.Lens ()
import Control.Monad (void)
import Data.Aeson ()
import qualified Data.Bifunctor
import qualified Data.Functor
import Data.Maybe (catMaybes, fromMaybe)
import Data.Semigroup (Option)
import Data.Time (diffUTCTime, getCurrentTime)
import MainHelpers
  ( ProgramArgs (..),
    normalApp,
    runQuizell,
  )
import Options.Applicative (auto, execParser, fullDesc, header, help, helper, info, long, metavar, option, progDesc, short, strOption, switch, value, (<**>))
import Options.Applicative.Types (Parser)
import qualified QuestionParser as QP
import System.Environment (getArgs, getEnv)
import System.Random (newStdGen)
import qualified Text.ParserCombinators.Parsec as P
import Text.Printf (printf)
import Text.Read (readEither, readMaybe)
import Utils (Log (toLog), getNumberOrDefault, numberStrings)

setQuizFile :: IO String
setQuizFile = do
  putStrLn "Enter Path to quiz file:"
  getLine

setQuizTimer :: IO Int
setQuizTimer = do
  putStrLn "Enter Amount of time for quiz in seconds (0 is infinity):"
  getNumberOrDefault 0

main :: IO ()
main = getDefaultQuizellArgs >>= setup >>= \x -> runQuizell x getWindowsUserName normalApp
  where
    setup args = do
      putStrLn "QUIZELL - WINDOWS\n"
      putStrLn $ numberStrings ["Set QuizFile", "Set Timer", "Show My Past Results", "Run"]
      putStrLn $ "Current Config: " ++ show args
      putStrLn "Enter Menu Option Number:"
      ent <- getNumberOrDefault (-1)
      case ent of
        1 -> do
          file <- setQuizFile
          setup $ args {quizPath = file}
        2 -> do
          qTime <- setQuizTimer
          setup $ args {time = qTime}
        3 -> do
          return $ args {showResults = True}
        4 -> return args
        _ -> putStrLn "Invalid Menu Option" >> setup args

getWindowsUserName :: IO String
getWindowsUserName = getEnv "USERNAME"

getDefaultQuizellArgs :: IO ProgramArgs
getDefaultQuizellArgs =
  return
    ProgramArgs
      { quizPath = "",
        quizLength = 0,
        showResults = False,
        time = -1,
        tuiOn = False
      }