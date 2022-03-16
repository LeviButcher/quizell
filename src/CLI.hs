{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module CLI where

import Control.Arrow (left)
import Control.Exception (try)
import Control.Monad (void)
import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.List.Zipper (toList)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import qualified QuestionParser as QP
import qualified Quiz as Q
import QuizResults (QuizResults (correct, taker, testFile, total), getResults)
import qualified QuizResults as QR
import System.Console.ANSI (Color (Blue, Green, Red), ColorIntensity (Dull, Vivid), ConsoleLayer (Background, Foreground), SGR (Reset, SetColor), clearScreen, setCursorPosition, setSGR)
import System.Random (RandomGen (split), newStdGen)
import System.Random.Shuffle (shuffle, shuffle')
import Text.Printf (printf)
import Text.Read (readMaybe)
import Utils (Log (readLog), allTrue, getNumberOrDefault, joinDelim, numberStrings)

type GetUserName m = m String

resetScreen :: IO ()
resetScreen = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

prettyText :: [SGR] -> (String -> IO ()) -> String -> IO ()
prettyText conf f x = do
  setSGR conf
  f x
  setSGR [Reset]
  return ()

errorText :: (String -> IO ()) -> String -> IO ()
errorText = prettyText [SetColor Foreground Vivid Red]

infoText :: (String -> IO ()) -> String -> IO ()
infoText = prettyText [SetColor Foreground Vivid Blue]

goodNewsText :: (String -> IO ()) -> String -> IO ()
goodNewsText = prettyText [SetColor Foreground Vivid Green]

formattedQuestion :: Q.Question -> String
formattedQuestion (Q.Question question answers _) =
  unlines ["Question: ", question, "\nAnswers:", numberedAnswers]
  where
    numberedAnswers = numberStrings answers

outputCorrectAnswer :: Q.Quiz q => q -> IO ()
outputCorrectAnswer q =
  do
    let x@(Q.Question _ _ ci, _) = Q.currAnswer q
    if Q.isCurrCorrect q
      then goodNewsText putStrLn "You answered correctly!"
      else do
        errorText putStrLn "You answered incorrectly."
        errorText putStrLn ("#" ++ show ci ++ " was the correct answer\n")
    >> putStrLn "Press Enter to continue"
    >> getLine
    >> return ()

askQuestion :: Q.Quiz q => q -> IO (Either Q.QuizError q)
askQuestion quiz = do
  putStrLn $ formattedQuestion (Q.curr quiz)
  putStrLn "Enter # of answer:"
  guess <- getNumberOrDefault (-1)
  return $ Q.answerCurr quiz guess

askQuestionUntilValid :: Q.Quiz q => q -> IO q
askQuestionUntilValid quiz = do
  res <- askQuestion quiz
  case res of
    Left err -> putStrLn "Invalid Answer, Try Again\n\n" >> askQuestionUntilValid quiz
    Right newState -> return newState

takeQuiz :: (Q.Quiz q) => GetUserName IO -> String -> q -> IO QuizResults
takeQuiz getUser file quiz = do
  resetScreen
  newState <- askQuestionUntilValid quiz

  outputCorrectAnswer newState
  if Q.hasNext quiz
    then takeQuiz getUser file (Q.next newState)
    else getResults <$> getUser <*> pure file <*> pure newState

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