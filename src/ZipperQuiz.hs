{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module ZipperQuiz
  ( ZipperQuiz,
  )
where

import Data.List.Zipper
  ( Zipper (..),
    beginp,
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
import Quiz (AnsweredQuestion, Direction (..), Question (..), QuestionList, Quiz (..), QuizError (..))
import System.Random (RandomGen (split))
import System.Random.Shuffle (shuffle')
import Utils (joinDelim)

type ZipperQuiz = Zipper AnsweredQuestion

instance Quiz ZipperQuiz where
  currAnswer = cursor

  next = right

  prev = left

  hasNext = not . endp . next

  hasPrev = not . beginp . prev

  answerCurr z i = do
    answered <- answerQuestion (fst . cursor $ z) i
    Right $ replace answered z

  directionAnswerCurr z i = do
    let answered = directionalAnswerQuestion (cursor z) i
    replace answered z

  total = length . toList
  totalAnswered = foldlz (\a z -> a + if isJust (snd $ cursor z) then 1 else 0) 0 . start
  totalCorrect = length . filter id . map isCorrect . toList

  createQuiz [] = Nothing
  createQuiz qs = Just $ fromList $ zip qs (repeat Nothing)

  currPosition (Zip [] []) = 0
  currPosition (Zip l _) = length l + 1

  isCurrCorrect = isCorrect . currAnswer

answerQuestion :: Question -> Int -> Either QuizError AnsweredQuestion
answerQuestion q@(Question _ a ci) ai
  | ai < 0 || ai > length a = Left InvalidAnswer
  | otherwise = Right (q, Just ai)

directionalAnswerQuestion :: AnsweredQuestion -> Direction -> AnsweredQuestion
directionalAnswerQuestion (q, Nothing) _ = (q, Just 1)
directionalAnswerQuestion (q@(Question _ a _), Just x) Up
  | (pred . length $ a) >= x = (q, Just $ x + 1)
  | otherwise = (q, Just 1)
directionalAnswerQuestion (q@(Question _ a _), Just x) Down
  | 0 < x = (q, Just $ x - 1)
  | otherwise = (q, Just 1)

isCorrect :: AnsweredQuestion -> Bool
isCorrect (_, Nothing) = False
isCorrect (Question _ _ c, Just x) = x == c
