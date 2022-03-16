module Main where

import Brick (defaultMain)
import Control.Concurrent (killThread, threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception (Exception, throw)
import qualified Control.Monad as Data.Foldable
import Data.Functor ((<&>))
import qualified Data.Functor
import MainHelpers (ProgramArgs (..), normalApp, runQuizell)
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

runProgram :: ProgramArgs -> IO ()
runProgram args = runQuizell args getLoginName $ if tuiOn args then tuiApp else normalApp

tuiApp :: ProgramArgs -> IO String -> Q.QuestionList -> IO (Maybe QR.QuizResults)
tuiApp args getUser questionList = do
  mQuiz <- startState questionList :: IO (Maybe (QuizState ZipperQuiz))
  sequence $
    mQuiz
      <&> ( \start -> do
              finalState <- defaultMain quizApp start
              QR.getResults (quizPath args) <$> getUser <*> pure (quiz finalState)
          )