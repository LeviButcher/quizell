{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Quiz where

import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.List.Zipper
  ( Zipper (..),
    cursor,
    endp,
    foldlz,
    fromList,
    left,
    replace,
    right,
    safeCursor,
    start,
    toList,
  )
import Data.Maybe (isJust)
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

type Quiz = Zipper AnsweredQuestion

data QuizError = InvalidAnswer

data Direction = Up | Down

answerQuestion :: Question -> Int -> Either QuizError AnsweredQuestion
answerQuestion q@(Question _ a ci) ai
  | ai < 0 || ai > length a = Left InvalidAnswer
  | otherwise = Right (q, pure ai)

answerCurrentQuestion :: Quiz -> Int -> Either QuizError Quiz
answerCurrentQuestion z i = do
  answered <- answerQuestion (fst . cursor $ z) i
  Right $ replace answered z

directionalAnswerQuestion :: AnsweredQuestion -> Direction -> AnsweredQuestion
directionalAnswerQuestion (q, Nothing) _ = (q, Just 1)
directionalAnswerQuestion (q@(Question _ a _), Just x) Up
  | (pred . length $ a) >= x = (q, Just $ x + 1)
  | otherwise = (q, Just 1)
directionalAnswerQuestion (q@(Question _ a _), Just x) Down
  | 0 < x = (q, Just $ x - 1)
  | otherwise = (q, Just 1)

directionalAnswerCurrentQuestion :: Direction -> Quiz -> Either QuizError Quiz
directionalAnswerCurrentQuestion i z = do
  let answered = directionalAnswerQuestion (cursor z) i
  Right $ replace answered z

nextQuestion :: Quiz -> Quiz
nextQuestion = right

prevQuestion :: Quiz -> Quiz
prevQuestion = left

-- End of quiz is when only one question is left in question zipper
isEndOfQuiz :: Quiz -> Bool
isEndOfQuiz (Zip l r) = (1 ==) . length $ l

currentQuestionNumber :: Quiz -> Int
currentQuestionNumber (Zip [] []) = 0
currentQuestionNumber (Zip l _) = length l + 1

totalQuestions :: Quiz -> Int
totalQuestions = length . toList

currentQuestion :: Quiz -> Question
currentQuestion q = fst $ cursor q

currentAnsweredQuestion :: Quiz -> AnsweredQuestion
currentAnsweredQuestion = cursor

isCorrect :: AnsweredQuestion -> Bool
isCorrect (_, Nothing) = False
isCorrect (Question _ _ c, Just x) = x == c

-- cursor guaranteed to be safe inside foldlz
totalAnswered :: Quiz -> Int
totalAnswered = foldlz (\a z -> a + if isJust (snd $ cursor z) then 1 else 0) 0 . start

totalCorrect :: Quiz -> Int
totalCorrect = length . filter id . map isCorrect . toList

startQuiz :: QuestionList -> Maybe Quiz
startQuiz [] = Nothing
startQuiz qs = Just $ fromList $ zip qs (repeat Nothing)

isLastQuestion :: Quiz -> Bool
isLastQuestion q = currentQuestionNumber q == totalQuestions q

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