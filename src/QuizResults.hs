module QuizResults where

import Control.Applicative (Alternative ((<|>)))
import qualified Quiz as Q
import System.Directory (createDirectory, createDirectoryIfMissing, doesFileExist, getAppUserDataDirectory)
import System.FilePath.Posix (takeDirectory)
import Utils (Log (readLog, toLog))

type Taker = String

type TestFile = String

-- TODO: add time taken
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

-- Don't want to override the Show typeclass
showResult :: QuizResults -> String
showResult (QuizResults a t c user tFile) =
  unlines
    [ "user: " ++ user,
      "file: " ++ tFile,
      "correct: " ++ show c ++ "/" ++ show t
    ]

showResults :: [QuizResults] -> String
showResults = unlines . map showResult

quizellLog :: String
quizellLog = "quizell"

resultsLog :: String
resultsLog = "results.log"

instance Log QuizResults where
  toLog res = do
    saveDir <- getAppUserDataDirectory quizellLog

    let filePath = saveDir ++ "/" ++ resultsLog
        createLog = writeFile filePath (show res)
        appendLog = appendFile filePath ("\n" ++ show res)

    createDirectoryIfMissing True saveDir
    fExist <- doesFileExist filePath
    if fExist then appendLog else createLog

  readLog = do
    saveDir <- getAppUserDataDirectory quizellLog
    let filePath = saveDir ++ "/" ++ resultsLog
    map read . lines <$> readFile filePath <|> pure []