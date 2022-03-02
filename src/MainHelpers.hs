module MainHelpers
  ( getQuizFile,
    getParsedQuiz,
    randomizeQuiz,
    trimQuiz,
    normalApp,
    transformFinishedQuiz,
    tuiApp,
  )
where

import Brick (defaultMain)
import qualified CLI
import Control.Arrow (left)
import Control.Exception (try)
import Data.Maybe (catMaybes)
import Data.Time (diffUTCTime, getCurrentTime)
import qualified Quiz as Q
import qualified QuizParser as QP
import System.Random (newStdGen)
import TUI (QuizState (..), quizApp, startState)
import Text.Printf (printf)
import Utils (getTimeString)

getQuizFile :: String -> IO (Either String String)
getQuizFile quizPath = do
  readResult <- try (readFile quizPath) :: IO (Either IOError String)
  return $ left (const $ "File entered does not exist: " ++ quizPath) readResult

getParsedQuiz :: Either String String -> IO (Either String Q.QuestionList)
getParsedQuiz x = return $ do
  r <- x
  left (const "Failed to parse file correctly\nPlease try again\n") $ QP.parseQuiz . QP.cleanQuizString $ r

randomizeQuiz :: Either String Q.QuestionList -> IO (Either String Q.QuestionList)
randomizeQuiz (Right quiz) = do
  rng <- newStdGen
  return . Right $ Q.shuffleQuestions rng quiz
randomizeQuiz l = return l

trimQuiz :: Int -> Either String Q.QuestionList -> Either String Q.QuestionList
trimQuiz _ (Left l) = Left l
trimQuiz n (Right q)
  | n < 0 = Left $ show n ++ " is a invalid number of questions"
  | n == 0 = Right q
  | n <= length q = Right $ take n q
  | otherwise = Left $ "Quiz only has " ++ show n ++ "Questions"

normalApp :: Q.QuestionList -> IO ()
normalApp qList = do
  let quiz = Q.startQuiz qList
  quizStart <- getCurrentTime
  finishedQuiz <- CLI.takeQuiz putStr getLine quiz
  quizEnd <- getCurrentTime
  CLI.presentResults putStrLn finishedQuiz
  let elapsedTime = diffUTCTime quizEnd quizStart
  putStrLn $ "Total Time: " ++ getTimeString elapsedTime

transformFinishedQuiz :: Q.AnsweredQuestion -> Maybe (Q.Question, Int)
transformFinishedQuiz (_, Nothing) = Nothing
transformFinishedQuiz (x, Just y) = Just (x, y + 1)

tuiApp :: Q.QuestionList -> IO ()
tuiApp quiz = do
  s <- startState quiz
  finalState <- defaultMain quizApp s
  -- let maybeAnswers = _answeredQuiz finalState
  -- let answers = catMaybes $ transformFinishedQuiz <$> maybeAnswers
  return ()