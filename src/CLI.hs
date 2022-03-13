{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module CLI where

import Control.Exception (try)
import Control.Monad (void)
import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.List.Zipper (toList)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Quiz
  ( AnsweredQuestion,
    Answers,
    Question (Question),
    QuestionList,
    Quiz,
    QuizError (EndOfQuiz, InvalidAnswer),
    answerCurrentQuestion,
    answerQuestion,
    currentAnsweredQuestion,
    currentQuestion,
    isCorrect,
    nextQuestion,
  )
import QuizResults (QuizResults (correct, taker, testFile, total), getResults)
import System.Random (RandomGen (split))
import System.Random.Shuffle (shuffle, shuffle')
import Text.Printf (printf)
import Text.Read (readMaybe)
import Utils (allTrue, joinDelim)

-- Note To Any Reader: I parameterized putStr and getLine in most functions for testing purposes
-- Hence why everything it taking a Monad m parameter

numberAnswers :: Answers -> String
numberAnswers s = unlines $ (\(a, b) -> concat [show a, ") ", b]) <$> zip [1 ..] s

outputQuestion :: Monad m => (String -> m ()) -> Question -> m ()
outputQuestion putStr (Question question answers _) = do
  let putStrLn = (\x -> putStr $ x ++ "\n")
      numberedAnswers = numberAnswers answers
  putStr "\nQuestion: "
  putStrLn question
  putStrLn "\nPossible Choices: "
  putStrLn numberedAnswers

outputCorrectAnswerInfo :: Monad m => (String -> m ()) -> m String -> Quiz -> m ()
outputCorrectAnswerInfo putStr getLine q =
  do
    case currentAnsweredQuestion q of
      Nothing -> return ()
      Just x@(Question _ _ ci, _) ->
        if isCorrect x
          then putStr "You answered correctly!!!"
          else
            putStr "You answered incorrectly"
              >> putStr ("#" ++ show ci ++ " was the correct answer\n")
    >> putStr "Press Enter to continue"
    >> getLine
    >> return ()

userAnswerQuestion :: Monad m => (String -> m ()) -> m String -> Quiz -> m (Either QuizError Quiz)
userAnswerQuestion putStr getLine quiz = do
  let curr = currentQuestion quiz
  case curr of
    Nothing -> return $ Left EndOfQuiz
    Just x@(Question question answers correctI) -> do
      let putStrLn = (\x -> putStr $ x ++ "\n")
      outputQuestion putStr x
      putStrLn "Enter # of answer:"
      guess <- fromMaybe 0 . readMaybe <$> getLine
      return $ answerCurrentQuestion quiz guess

type GetLine m = m String

type PutStr m = (String -> m ())

type GetUserName m = m String

takeQuiz :: Monad m => GetUserName m -> PutStr m -> GetLine m -> String -> Quiz -> m QuizResults
takeQuiz getUser putStr getLine file q = do
  let putStrLn = (\x -> putStr $ x ++ "\n")
  res <- userAnswerQuestion putStr getLine q

  case res of
    Left err -> case err of
      EndOfQuiz -> getResults <$> getUser <*> pure file <*> pure q
      InvalidAnswer -> putStrLn "Invalid Answer\nTry Again\n\n" >> takeQuiz getUser putStr getLine file q
    Right zip -> do
      outputCorrectAnswerInfo putStrLn getLine zip -- Output answer info before moving to next questions
      takeQuiz getUser putStr getLine file (nextQuestion zip)

presentResults :: Monad m => PutStr m -> QuizResults -> m ()
presentResults putStrLn qr = do
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
