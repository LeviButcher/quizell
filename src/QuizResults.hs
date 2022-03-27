module QuizResults where

import Control.Applicative (Alternative ((<|>)))
import Data.Time (UTCTime, diffUTCTime)
import qualified Quiz as Q
import System.Directory (XdgDirectoryList (XdgDataDirs), createDirectory, createDirectoryIfMissing, doesFileExist, getAppUserDataDirectory, getXdgDirectoryList)
import System.FilePath.Posix (takeDirectory)
import Utils (Log (readLog, toLog))

type Taker = String

type TestFile = String

data QuizResults = QuizResults
  { answered :: Int,
    total :: Int,
    correct :: Int,
    taker :: Taker,
    testFile :: TestFile,
    startTime :: UTCTime,
    endTime :: UTCTime,
    allotedTime :: Int
  }
  deriving (Show, Read)

getResults :: Taker -> TestFile -> Q.Quiz -> UTCTime -> UTCTime -> Int -> QuizResults
getResults t tf q start end alloted =
  QuizResults
    { answered = Q.totalAnswered q,
      total = Q.total q,
      correct = Q.totalCorrect q,
      taker = t,
      testFile = tf,
      startTime = start,
      endTime = end,
      allotedTime = alloted
    }

-- Don't want to override the Show typeclass
showResult :: QuizResults -> String
showResult (QuizResults a t c user tFile start end alloted) =
  unlines
    [ "user: " ++ user,
      "file: " ++ tFile,
      "correct: " ++ show c ++ "/" ++ show t ++ " => " ++ show percentC ++ "%",
      "answered: " ++ show a ++ "/" ++ show t,
      "time taken: " ++ show (diffUTCTime end start),
      if alloted > 0 then "time allowed: " ++ show alloted else ""
    ]
  where
    percentC = fromIntegral c / fromIntegral t * 100

showResults :: [QuizResults] -> String
showResults = unlines . map showResult

quizellLog :: String
quizellLog = "quizell"

resultsLog :: String
resultsLog = "results.log"

getLinuxStorage, getWindowsStorage :: FilePath -> IO FilePath
getLinuxStorage s = do
  let base = "/usr/share"
  return $ mconcat [base, "/", s]
getWindowsStorage s = do
  let base = "C:/"
  return $ mconcat [base, "/", s]

instance Log QuizResults where
  toLog getStorage res = do
    saveDir <- getStorage quizellLog

    let filePath = saveDir ++ "/" ++ resultsLog
        createLog = writeFile filePath (show res)
        appendLog = appendFile filePath ("\n" ++ show res)

    createDirectoryIfMissing True saveDir
    fExist <- doesFileExist filePath
    if fExist then appendLog else createLog

  readLog getStorage = do
    saveDir <- getStorage quizellLog
    let filePath = saveDir ++ "/" ++ resultsLog
    map read . lines <$> readFile filePath <|> pure []