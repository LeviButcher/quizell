{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Quiz (Quiz (..), shuffleQuestions, toWindowsFileString, toPOSIXFileString, Question (..), QuestionList, AnsweredQuestion, Direction (..), QuizError (..)) where

import Control.Monad.State.Lazy
import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (FromJSON (parseJSON))
import GHC.Generics (Generic)
import System.Random (RandomGen (split))
import System.Random.Shuffle (shuffle')
import Utils (joinDelim)

type Answers = [String]

data Question = Question
  { question :: String,
    answers :: Answers,
    correctAnswer :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON Question where
  parseJSON (Object v) =
    Question <$> v .: "question"
      <*> v .: "answers"
      <*> v .: "correctAnswer"

type QuestionList = [Question]

type AnsweredQuestion = (Question, Maybe Int)

-- Guarantees a different order of the quiz passed in
shuffleQuestions :: RandomGen gen => gen -> QuestionList -> QuestionList
shuffleQuestions g [] = []
shuffleQuestions g [x] = [x]
shuffleQuestions g q =
  let shuffled = shuffle' q (length q) g
   in if shuffled /= q then shuffled else shuffleQuestions (fst . split $ g) q

toFileString :: String -> QuestionList -> String
toFileString delim quiz = toString =<< quiz
  where
    join_ = joinDelim delim
    toString (Question a b c) =
      join_
        [ "@QUESTION",
          a,
          "@ANSWERS",
          show c,
          join_ b,
          "@END"
        ]

toWindowsFileString :: QuestionList -> String
toWindowsFileString = toFileString "\r\n"

toPOSIXFileString :: QuestionList -> String
toPOSIXFileString = toFileString "\n"

data QuizError = InvalidAnswer

data Direction = Up | Down

class Quiz a where
  curr :: a -> Question
  curr = fst . currAnswer

  currAnswer :: a -> AnsweredQuestion
  next :: a -> a
  prev :: a -> a
  hasNext :: a -> Bool
  hasPrev :: a -> Bool
  currPosition :: a -> Int
  answerCurr :: a -> Int -> Either QuizError a
  directionAnswerCurr :: a -> Direction -> a -- Can probably define via answerCurr
  total :: a -> Int
  totalAnswered :: a -> Int
  totalCorrect :: a -> Int
  createQuiz :: QuestionList -> Maybe a
  isCurrCorrect :: a -> Bool
