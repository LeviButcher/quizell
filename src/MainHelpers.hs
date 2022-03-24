module MainHelpers (ProgramArgs (..), runQuizell, normalApp) where

import Control.Concurrent (MVar, newMVar, threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception (Exception, throw)
import qualified Control.Monad as Data.Foldable
import Control.Monad.ST (runST)
import Control.Monad.State.Lazy (StateT (runStateT), execStateT)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Time (diffUTCTime, getCurrentTime)
import qualified QuestionParser as QP
import qualified Quiz as Q
import QuizCLI (getQuestionList, printUserLogs, takeQuiz)
import QuizResults (QuizResults, getResults)
import qualified QuizResults as QR
import System.Random (newStdGen)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Utils (Log (readLog, toLog), getTimeString)

data ProgramArgs = ProgramArgs
  { quizPath :: String,
    quizLength :: Int,
    tuiOn :: Bool,
    showResults :: Bool,
    time :: Int
  }
  deriving (Show)

type QuizellApp = ProgramArgs -> Q.QuestionList -> IO Q.Quiz

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
  -- res <- if time <= 0 then Right <$> actualProgram else race (quizStopper time) actualProgram
  -- case res of
  --   Left _ -> putStrLn "TIMES UP, TRY NEXT TIME KID"
  --   Right _ -> putStrLn "Thank you for using quizell"
  actualProgram
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
                  quizStart <- getCurrentTime
                  finalState <- quizell args quiz
                  res <- getResults <$> getName <*> pure q <*> pure finalState
                  quizEnd <- getCurrentTime
                  let elapsedTime = diffUTCTime quizEnd quizStart
                  QR.presentResults res
                  toLog res
          )

normalApp :: ProgramArgs -> Q.QuestionList -> IO Q.Quiz
normalApp args qList = do
  let quiz = Q.createQuiz qList
  execStateT takeQuiz quiz

-- asyncNormalApp :: ProgramArgs -> IO String -> Q.QuestionList -> IO (MVar Q.Quiz)
-- asyncNormalApp args getUserName qList = do
--   quiz <- newMVar (Q.createQuiz qList)
--   execStateT takeQuiz quiz