{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib
  ( Question,
    Answers,
    Quiz,
    QuizQuestion (..),
    QuizQuestions,
    takeQuiz,
    shuffleQuiz,
    presentResults,
    toWindowsFileString,
    toPOSIXFileString,
    numberAnswers,
    numberCorrect,
  )
where

import Control.Exception (try)
import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import System.Random (RandomGen (split))
import System.Random.Shuffle (shuffle, shuffle')
import Text.Printf (printf)
import Text.Read (readMaybe)
import Utils (allTrue, joinDelim)

type Question = String

type Answers = [String]

type QuizQuestions = [QuizQuestion]

data QuizQuestion = QuizQuestion
  { question :: Question,
    answers :: Answers,
    correctAnswer :: Int
  }
  deriving (Show, Generic)

instance Eq QuizQuestion where
  (==) (QuizQuestion a b c) (QuizQuestion aa bb cc) = allTrue [a == aa, b == bb, c == cc]

type AnsweredQuestion = (Int, QuizQuestion)

type AnsweredQuizQuestions = [AnsweredQuestion]

data Quiz = Quiz
  { questions :: QuizQuestions,
    answerList :: [Maybe Int],
    currentQuestion :: Int,
    finishedQuiz :: Bool
  }

startQuiz :: QuizQuestions -> Quiz
startQuiz s = Quiz s (replicate (length s) Nothing) 1 False

finishQuiz :: Quiz -> Quiz
finishQuiz (Quiz q a c _) = Quiz q a c True

answerQuestion' :: Quiz -> Int -> Quiz
answerQuestion' = const

instance FromJSON QuizQuestion where
  parseJSON (Object v) =
    QuizQuestion <$> v .: "question"
      <*> v .: "answers"
      <*> v .: "correctAnswer"

-- Guarantees a different order of the quiz passed in
shuffleQuiz :: RandomGen gen => gen -> QuizQuestions -> QuizQuestions
shuffleQuiz g [] = []
shuffleQuiz g [x] = [x]
shuffleQuiz g q =
  let shuffled = shuffle' q (length q) g
   in if shuffled /= q then shuffled else shuffleQuiz (fst . split $ g) q

numberAnswers :: Answers -> String
numberAnswers s = unlines $ (\(a, b) -> concat [show a, ") ", b]) <$> zip [1 ..] s

answerQuestion :: Monad m => (String -> m ()) -> m String -> QuizQuestion -> m AnsweredQuestion
answerQuestion putStr getLine x = do
  let putStrLn = (\x -> putStr $ x ++ "\n")
      (QuizQuestion question answers correctAnswer) = x
      numberedAnswers = numberAnswers answers
  putStr "\nQuestion: "
  putStrLn question
  putStrLn "\nPossible Choices: "
  putStrLn numberedAnswers
  putStrLn "Enter # of answer:"
  guess <- fromMaybe 0 . readMaybe <$> getLine

  if guess <= 0 || guess > length answers
    then putStrLn "Invalid Answer, please try again." >> answerQuestion putStr getLine x
    else do
      if guess == correctAnswer
        then putStrLn "\nYou answered Correct!!!\n"
        else
          putStrLn "\nThat answer was incorrect... :("
            >> putStrLn ("Correct Answer: " ++ show correctAnswer ++ "\n\n")

      putStrLn "Hit Enter for next question:"
      getLine

      return (guess, x)

-- This could be a fold operation
takeQuiz :: Monad m => (String -> m ()) -> m String -> QuizQuestions -> m AnsweredQuizQuestions
takeQuiz putStr getLine = mapM (answerQuestion putStr getLine)

numberCorrect :: AnsweredQuizQuestions -> Int
numberCorrect =
  foldr
    (\(selected, QuizQuestion _ _ correct) acc -> if selected == correct then acc + 1 else acc)
    0

presentResults :: Monad m => (String -> m ()) -> AnsweredQuizQuestions -> m ()
presentResults putStrLn aq = do
  let quizLength = length aq
      correct = numberCorrect aq
      percent = fromIntegral correct / fromIntegral quizLength * 100
  putStrLn $ "Total Correct: " ++ show correct
  putStrLn $ "Total Questions: " ++ show quizLength
  putStrLn $ "Percentage Correct: " ++ show percent ++ "%"

toFileString :: String -> QuizQuestions -> String
toFileString delim quiz = toString =<< quiz
  where
    join_ = joinDelim delim
    toString (QuizQuestion a b c) =
      join_
        [ "@QUESTION",
          a,
          "@ANSWERS",
          show c,
          join_ b,
          "@END"
        ]

toWindowsFileString :: [QuizQuestion] -> String
toWindowsFileString = toFileString "\r\n"

toPOSIXFileString :: [QuizQuestion] -> String
toPOSIXFileString = toFileString "\n"