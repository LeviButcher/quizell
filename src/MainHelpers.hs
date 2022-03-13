module MainHelpers
  ( getQuizFile,
    getParsedQuiz,
    randomizeQuiz,
    trimQuiz,
    normalApp,
    transformFinishedQuiz,
  )
where

import qualified CLI
import Control.Arrow (left)
import Control.Exception (try)
import Data.Maybe (catMaybes)
import Data.Time (diffUTCTime, getCurrentTime)
import qualified QuestionParser as QP
import qualified Quiz as Q
import QuizResults (QuizResults)
import System.Random (newStdGen)
import Text.Printf (printf)
import Utils (getTimeString)
import ZipperQuiz

getQuizFile :: String -> IO (Either String String)
getQuizFile quizPath = do
  readResult <- try (readFile quizPath) :: IO (Either IOError String)
  return $ left (const $ "File entered does not exist: " ++ quizPath) readResult

getParsedQuiz :: Either String String -> IO (Either String Q.QuestionList)
getParsedQuiz x = return $ do
  r <- x
  left (const "Failed to parse file correctly\nPlease try again\n") $ QP.parseQuestions r

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

normalApp :: String -> IO String -> Q.QuestionList -> IO (Maybe QuizResults)
normalApp file getUserName qList = do
  let maybeQ = Q.createQuiz qList :: Maybe ZipperQuiz

  case maybeQ of
    Nothing -> putStrLn "Quiz List was Empty" >> return Nothing
    Just quiz -> do
      quizStart <- getCurrentTime
      finishedQuiz <- CLI.takeQuiz getUserName putStr getLine file quiz
      quizEnd <- getCurrentTime
      CLI.presentResults putStrLn finishedQuiz
      let elapsedTime = diffUTCTime quizEnd quizStart
      putStrLn $ "Total Time: " ++ getTimeString elapsedTime
      return . Just $ finishedQuiz

transformFinishedQuiz :: Q.AnsweredQuestion -> Maybe (Q.Question, Int)
transformFinishedQuiz (_, Nothing) = Nothing
transformFinishedQuiz (x, Just y) = Just (x, y + 1)