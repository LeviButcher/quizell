module Quiz where

import Control.Concurrent.MState
  ( MState,
    MonadState (get, put),
    modifyM,
  )
import Control.Monad.Cont (MonadIO)
import Data.List.Zipper (Zipper (Zip), cursor, endp, foldlz, fromList, left, replace, right, start, toList)
import qualified Data.List.Zipper as Z
import Data.Maybe (fromMaybe, isJust)
import System.Random (RandomGen (split))
import System.Random.Shuffle (shuffle')

data Question = Question
  { question :: String,
    answers :: [String],
    correct :: Int
  }
  deriving (Show, Eq)

type QuestionList = [Question]

type AnsweredQuestion = (Question, Maybe Int)

type Quiz = Zipper AnsweredQuestion

data Direction = Up | Down

next, prev :: Quiz -> Quiz
next = right
prev = left

curr :: Quiz -> Question
curr = fst <$> currAnswer

currAnswer :: Quiz -> AnsweredQuestion
currAnswer = cursor

hasNext, hasPrev :: Quiz -> Bool
hasNext = not . endp . Z.right
hasPrev = not . Z.beginp . Z.left

validAnswer :: Int -> Question -> Bool
validAnswer n (Question _ ans _) = n >= 1 && n <= totalAnswers
  where
    totalAnswers = length ans

answerCurr :: Int -> Quiz -> Maybe Quiz
answerCurr n q
  | validAnswer n current = Just (replace (current, Just n) q)
  | otherwise = Nothing
  where
    current = curr q

directionalAnswerCurr :: Direction -> Quiz -> Quiz
directionalAnswerCurr d quiz =
  let (Question _ a ci, maybe) = currAnswer quiz
      totalAnswers = length a
      curr = fromMaybe 0 maybe
      answer = direction d curr totalAnswers
   in fromMaybe quiz (answerCurr answer quiz)
  where
    direction Up curr totalAnswers =
      if curr < totalAnswers
        then succ curr
        else 1
    direction Down curr totalAnswers =
      if curr <= 1
        then totalAnswers
        else pred curr

isCorrect :: AnsweredQuestion -> Bool
isCorrect (_, Nothing) = False
isCorrect (q, a) = pure (correct q) == a

total :: Quiz -> Int
total = length . toList

totalAnswered :: Quiz -> Int
totalAnswered = foldlz (\a z -> a + if isJust (snd $ cursor z) then 1 else 0) 0 . start

totalCorrect :: Quiz -> Int
totalCorrect = length . filter id . map isCorrect . toList

currPosition :: Quiz -> Int
currPosition (Zip [] []) = 0
currPosition (Zip l _) = length l + 1


createShuffledQuizToLength :: (RandomGen gen) => gen -> Int -> [Question] -> Quiz
createShuffledQuizToLength gen 0 = createQuiz . shuffleQuestions gen
createShuffledQuizToLength gen n = createQuiz . take n . shuffleQuestions gen

createQuiz :: [Question] -> Quiz
createQuiz qs = fromList $ zip qs (repeat Nothing)

shuffleQuestions :: (RandomGen gen) => gen -> QuestionList -> QuestionList
shuffleQuestions g [] = []
shuffleQuestions g [x] = [x]
shuffleQuestions g q =
  let shuffled = shuffle' q (length q) g
   in if shuffled /= q then shuffled else shuffleQuestions (fst . split $ g) q