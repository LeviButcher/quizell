module MainHelpers (ProgramArgs (..), runQuizell, normalApp) where

import CLI (getQuestionList, printUserLogs)
import qualified CLI
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception (Exception, throw)
import qualified Control.Monad as Data.Foldable
import Data.Maybe (catMaybes, fromMaybe)
import Data.Time (diffUTCTime, getCurrentTime)
import qualified QuestionParser as QP
import qualified Quiz as Q
import QuizResults (QuizResults)
import qualified QuizResults as QR
import System.Random (newStdGen)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Utils (Log (readLog, toLog), getTimeString)
import ZipperQuiz (ZipperQuiz)

data ProgramArgs = ProgramArgs
  { quizPath :: String,
    quizLength :: Int,
    tuiOn :: Bool,
    showResults :: Bool,
    time :: Int
  }
  deriving (Show)

type QuizellApp = ProgramArgs -> IO String -> Q.QuestionList -> IO (Maybe QR.QuizResults)

data QuizException = OutOfTime deriving (Show)

instance Exception QuizException

quizStopper :: Int -> IO ()
quizStopper n = do
  threadDelay . secondsToMicro $ n
  throw OutOfTime

secondsToMicro :: Num a => a -> a
secondsToMicro x = x * 1000000

runQuizell :: ProgramArgs -> IO String -> QuizellApp -> IO ()
runQuizell args@(ProgramArgs q l t r time) getName quizell = do
  res <- if time <= 0 then Right <$> actualProgram else race (quizStopper time) actualProgram
  case res of
    Left _ -> putStrLn "TIMES UP, TRY NEXT TIME KID"
    Right _ -> putStrLn "Thank you for using quizell"
  where
    actualProgram = do
      if r
        then printUserLogs getName
        else
          ( do
              qList <- getQuestionList q l
              case qList of
                Left err -> putStrLn err
                Right quiz -> do
                  res <- quizell args getName quiz
                  Data.Foldable.forM_ res toLog
          )

normalApp :: ProgramArgs -> IO String -> Q.QuestionList -> IO (Maybe QuizResults)
normalApp args getUserName qList = do
  let maybeQ = Q.createQuiz qList :: Maybe ZipperQuiz

  case maybeQ of
    Nothing -> putStrLn "Quiz List was Empty" >> return Nothing
    Just quiz -> do
      quizStart <- getCurrentTime
      finishedQuiz <- CLI.takeQuiz getUserName (quizPath args) quiz
      quizEnd <- getCurrentTime
      CLI.presentResults finishedQuiz
      let elapsedTime = diffUTCTime quizEnd quizStart
      putStrLn $ "Total Time: " ++ getTimeString elapsedTime
      return . Just $ finishedQuiz
