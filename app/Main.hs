module Main where

import Brick (defaultMain)
import Control.Concurrent (killThread, threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception (Exception, throw)
import qualified Control.Monad as Data.Foldable
import Data.Functor ((<&>))
import qualified Data.Functor
import MainHelpers
  ( getQuestionList,
    normalApp,
    printUserLogs,
  )
import Options.Applicative
  ( Parser,
    auto,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    progDesc,
    short,
    strOption,
    switch,
    value,
    (<**>),
  )
import Options.Applicative.Types (Parser)
import qualified QuestionParser as QP
import Quiz (QuestionList)
import qualified Quiz as Q
import qualified QuizResults as QR
import System.Posix.User (getLoginName)
import TUI (QuizState (..), quizApp, startState)
import Utils (Log (readLog), toLog)
import ZipperQuiz (ZipperQuiz)

data ProgramArgs = ProgramArgs
  { quizPath :: String,
    quizLength :: Int,
    tuiOn :: Bool,
    showResults :: Bool,
    time :: Int
  }

parseArgs :: Parser ProgramArgs
parseArgs =
  ProgramArgs
    <$> strOption (long "file" <> short 'f' <> metavar "Quiz File Path" <> help "Full or Relative path to Quiz file")
    <*> option auto (long "length" <> short 'l' <> help "Number of questions to use" <> metavar "INT" <> value 0)
    <*> switch (long "tui" <> help "Turn on TUI mode (Works only on Unix)")
    <*> switch (long "results" <> short 'r' <> help "Show your past quiz results")
    <*> option auto (long "time" <> short 't' <> help "Amount of time for quiz (In Seconds)" <> metavar "INT" <> value 0)

main :: IO ()
main = runProgram =<< execParser opts
  where
    opts =
      info
        (parseArgs <**> helper)
        ( fullDesc
            <> progDesc "quizell - a CLI for quiz taking"
            <> header "quizell"
        )

data QuizException = OutOfTime deriving (Show)

instance Exception QuizException

secondsToMicro x = x * 1000000

quizStopper :: Int -> IO ()
quizStopper n = do
  threadDelay . secondsToMicro $ n
  throw OutOfTime

runProgram :: ProgramArgs -> IO ()
runProgram (ProgramArgs q l t r time) = do
  res <- race (quizStopper time) actualProgram
  case res of
    Left _ -> putStrLn "TIMES UP, TRY NEXT TIME KID"
    Right _ -> putStrLn "Thank you for using quizell"
  where
    actualProgram = do
      if r
        then printUserLogs getLoginName
        else
          ( do
              qList <- getQuestionList q l
              case qList of
                Left err -> putStrLn err
                Right quiz -> do
                  let prog = if t then tuiApp else normalApp
                  res <- prog q getLoginName quiz
                  Data.Foldable.forM_ res toLog
          )

tuiApp :: String -> IO String -> Q.QuestionList -> IO (Maybe QR.QuizResults)
tuiApp q getUser questionList = do
  mQuiz <- startState questionList :: IO (Maybe (QuizState ZipperQuiz))
  sequence $
    mQuiz
      <&> ( \sQuiz -> do
              finalState <- defaultMain quizApp sQuiz
              QR.getResults q <$> getUser <*> pure (quiz finalState)
          )