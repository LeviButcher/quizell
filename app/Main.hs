module Main where

import Brick (defaultMain)
import MainHelpers
  ( getParsedQuiz,
    getQuizFile,
    normalApp,
    randomizeQuiz,
    trimQuiz,
    tuiApp,
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
import qualified QuizParser as QP
import TUI (QuizState, quizApp, startState)

data ProgramArgs = ProgramArgs
  { quizPath :: String,
    quizLength :: Int,
    tuiOn :: Bool
  }

parseArgs :: Parser ProgramArgs
parseArgs =
  ProgramArgs
    <$> strOption (long "file" <> short 'f' <> metavar "Quiz File Path" <> help "Full or Relative path to Quiz file")
    <*> option auto (long "length" <> short 'l' <> help "Number of questions to use" <> metavar "INT" <> value 0)
    <*> switch (long "tui" <> short 't' <> help "Turn on TUI mode (Works only on Unix)")

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
runProgram (ProgramArgs q l t) = do
  pq <- getQuizFile q >>= getParsedQuiz >>= randomizeQuiz
  let pq2 = trimQuiz l pq
  case pq2 of
    Left err -> putStrLn err
    Right quiz -> do
      let prog = if t then tuiApp else normalApp
      prog quiz
