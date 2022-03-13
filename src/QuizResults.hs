module QuizResults where

import Control.Applicative (Alternative ((<|>)))
import qualified Quiz as Q
import System.Directory (createDirectory, createDirectoryIfMissing, getAppUserDataDirectory)
import System.FilePath.Posix (takeDirectory)

type Taker = String

type TestFile = String

quizellLog :: String
quizellLog = "quizell/quizell.log"

-- Should add time taken... :/
data QuizResults = QuizResults
  { answered :: Int,
    total :: Int,
    correct :: Int,
    taker :: Taker,
    testFile :: TestFile
  }
  deriving (Show, Read)

getResults :: Q.Quiz q => Taker -> TestFile -> q -> QuizResults
getResults t tf q =
  QuizResults
    { answered = Q.totalAnswered q,
      total = Q.total q,
      correct = Q.totalCorrect q,
      taker = t,
      testFile = tf
    }

toSystemLog :: String -> QuizResults -> IO ()
toSystemLog file q =
  do
    savePath <- getAppUserDataDirectory file
    let createLog = createDirectory (takeDirectory savePath) *> writeFile savePath (show q)
        appendLog = appendFile savePath ("\n" ++ show q) -- Should switch this to use correct line delimiters
    createLog <|> appendLog

toUnixLog, toWindowsLog, toLog :: QuizResults -> IO ()
toUnixLog = toSystemLog quizellLog
toWindowsLog = toSystemLog quizellLog
toLog = toSystemLog quizellLog