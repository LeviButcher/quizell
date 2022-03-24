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

getResults :: Taker -> TestFile -> Q.Quiz -> QuizResults
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

presentResults :: QuizResults -> IO ()
presentResults qr = do
  let totalQ = total qr
      correctQ = correct qr
      percentC = fromIntegral correctQ / fromIntegral totalQ * 100
      file = testFile qr
      user = taker qr
  putStrLn $ "Results for: " ++ user
  putStrLn $ "Total Correct: " ++ show correctQ
  putStrLn $ "Total Questions: " ++ show totalQ
  putStrLn $ "Percentage Correct: " ++ show percentC ++ "%"
  putStrLn $ "Test File: " ++ file