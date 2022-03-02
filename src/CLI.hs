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
    QuizResults (correct, total),
    answerCurrentQuestion,
    answerQuestion,
    currentAnsweredQuestion,
    currentQuestion,
    getResults,
    isCorrect,
    nextQuestion,
  )
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

takeQuiz :: Monad m => (String -> m ()) -> m String -> Quiz -> m QuizResults
takeQuiz putStr getLine q = do
  let putStrLn = (\x -> putStr $ x ++ "\n")
  res <- userAnswerQuestion putStr getLine q

  case res of
    Left err -> case err of
      EndOfQuiz -> return $ getResults q
      InvalidAnswer -> putStrLn "Invalid Answer\nTry Again\n\n" >> takeQuiz putStr getLine q
    Right zip -> do
      outputCorrectAnswerInfo putStrLn getLine zip -- Output answer info before moving to newt questions
      takeQuiz putStr getLine (nextQuestion zip)

presentResults :: Monad m => (String -> m ()) -> QuizResults -> m ()
presentResults putStrLn qr = do
  let totalQ = total qr
      correctQ = correct qr
      percentC = fromIntegral correctQ / fromIntegral totalQ * 100
  putStrLn $ "Total Correct: " ++ show correctQ
  putStrLn $ "Total Questions: " ++ show totalQ
  putStrLn $ "Percentage Correct: " ++ show percentC ++ "%"
