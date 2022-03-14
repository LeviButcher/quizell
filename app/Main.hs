module Main where

import Brick (defaultMain)
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
    showResults :: Bool
  }

parseArgs :: Parser ProgramArgs
parseArgs =
  ProgramArgs
    <$> strOption (long "file" <> short 'f' <> metavar "Quiz File Path" <> help "Full or Relative path to Quiz file")
    <*> option auto (long "length" <> short 'l' <> help "Number of questions to use" <> metavar "INT" <> value 0)
    <*> switch (long "tui" <> short 't' <> help "Turn on TUI mode (Works only on Unix)")
    <*> switch (long "results" <> short 'r' <> help "Show your past quiz results")

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

runProgram :: ProgramArgs -> IO ()
runProgram (ProgramArgs q l t r) = do
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