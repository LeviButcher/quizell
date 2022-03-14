{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module MainHelpers
  ( normalApp,
    getQuestionList,
    printUserLogs,
  )
where

import qualified CLI
import Control.Applicative ((<|>))
import Control.Arrow (left)
import Control.Exception (try)
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Data.Time (diffUTCTime, getCurrentTime)
import qualified QuestionParser as QP
import qualified Quiz as Q
import QuizResults (QuizResults)
import qualified QuizResults as QR
import System.Random (newStdGen)
import Text.Printf (printf)
import Utils (Log (readLog), getTimeString)
import ZipperQuiz (ZipperQuiz)

getQuizFile :: String -> IO (Either String String)
getQuizFile quizPath = do
  readResult <- try (readFile quizPath) :: IO (Either IOError String)
  return $ left (const $ "File entered does not exist: " ++ quizPath) readResult

getParsedQuiz :: String -> Either String Q.QuestionList
getParsedQuiz = first (const "Failed to parse file correctly\nPlease try again\n") . QP.parseQuestions

randomizeQuiz :: Q.QuestionList -> IO Q.QuestionList
randomizeQuiz qList = do
  rng <- newStdGen
  return $ Q.shuffleQuestions rng qList

trimQuiz :: Int -> Q.QuestionList -> Either String Q.QuestionList
trimQuiz n q
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

printUserLogs :: IO String -> IO ()
printUserLogs getName = do
  user <- getName
  logs <- readLog
  let userLogs = filter ((user ==) . QR.taker) logs
  putStrLn $ QR.showResults userLogs

getQuestionList :: FilePath -> Int -> IO (Either String Q.QuestionList)
getQuestionList file n = do
  txt <- getQuizFile file
  randomQ <- sequence $ txt >>= getParsedQuiz <&> randomizeQuiz
  return $ randomQ >>= trimQuiz n
