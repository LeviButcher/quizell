module MainHelpers (ProgramArgs (..), runQuizell, normalApp) where

import Control.Concurrent (MVar, newMVar, threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.MState (execMState)
import Control.Exception (Exception, throw)
import qualified Control.Monad as Data.Foldable
import Control.Monad.ST (runST)
import Control.Monad.State.Lazy (StateT (runStateT), execStateT)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Time (diffUTCTime, getCurrentTime)
import qualified QuestionParser as QP
import qualified Quiz as Q
import QuizCLI (getQuestionList, printUserLogs, takeQuiz, timedTakeQuiz)
import QuizResults (QuizResults, getResults)
import qualified QuizResults as QR
import System.Random (newStdGen)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Utils (Log (readLog, toLog), getTimeString)
import Control.Monad.Except

data ProgramArgs = ProgramArgs
  { quizPath :: String,
    quizLength :: Int,
    tuiOn :: Bool,
    showResults :: Bool,
    time :: Int
  }
  deriving (Show)

type QuizellApp = ProgramArgs -> Q.QuestionList -> IO (Either String Q.Quiz)

data QuizException = OutOfTime deriving (Show)

instance Exception QuizException

quizStopper :: Int -> IO ()
quizStopper n = do
  threadDelay . secondsToMicro $ n
  throw OutOfTime

secondsToMicro :: Num a => a -> a
secondsToMicro x = x * 1000000

runQuizell :: ProgramArgs -> (String -> IO String) -> IO String -> QuizellApp -> IO ()
runQuizell args@(ProgramArgs q l t r time) getStorage getName quizell = do
    if r
      then printUserLogs getStorage getName
      else do
        mainRes <- mainPath
        case mainRes of
          Left err -> print err
          Right _ -> return ()
  where 
    mainPath = runExceptT $ do
      qList <- ExceptT $ getQuestionList q
      quizStart <- liftIO $ getCurrentTime
      finalState <- ExceptT $ quizell args qList
      res <-
        liftIO $ getResults <$> getName <*> pure q <*> pure finalState <*> pure quizStart
          <*> getCurrentTime
          <*> pure time
      liftIO $ putStrLn "--QUIZ RESULTS--"
      liftIO $ putStrLn (QR.showResult res)
      liftIO $ toLog getStorage res
      liftIO $ putStrLn "Press Enter to Cont."
      liftIO $ getLine
      return ()

normalApp :: ProgramArgs -> Q.QuestionList -> IO (Either String Q.Quiz)
normalApp args qList = runExceptT $ do
  rng <- liftIO newStdGen
  quiz <- liftEither $ Q.createShuffledQuizToLength rng (quizLength args) qList
  let t = time args

  if t > 0
    then liftIO $ execMState (timedTakeQuiz (secondsToMicro t)) quiz
    else liftIO $ execMState takeQuiz quiz