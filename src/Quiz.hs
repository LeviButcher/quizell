module Quiz where

import Control.Monad.State.Lazy (MonadState (get, put), MonadTrans (lift), State, StateT)
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

type QuizTaker m a = StateT Quiz m a

next :: (Monad m) => QuizTaker m ()
next = do
  s <- get
  put . right $ s

prev :: (Monad m) => QuizTaker m ()
prev = do
  s <- get
  put . left $ s

curr :: (Monad m) => QuizTaker m Question
curr = fst <$> currAnswer

currAnswer :: (Monad m) => QuizTaker m AnsweredQuestion
currAnswer = cursor <$> get

hasNext :: (Monad m) => QuizTaker m Bool
hasNext = do
  not . endp . Z.right <$> get

hasPrev :: (Monad m) => QuizTaker m Bool
hasPrev = do
  not . Z.beginp . Z.left <$> get

answerCurr :: (Monad m) => Int -> QuizTaker m (Int, Int)
answerCurr n = do
  s <- get
  x <- curr
  put $ replace (x, Just n) s
  return (n, correct x)

-- Can easily reduce this down
directionalAnswerCurr :: (Monad m) => Direction -> QuizTaker m (Int, Int)
directionalAnswerCurr d = do
  (Question _ a ci, maybe) <- currAnswer
  let totalAnswers = length a
  let curr = fromMaybe 0 maybe
  let answer = direction d curr totalAnswers
  answerCurr answer
  return (answer, ci)
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

createQuiz :: [Question] -> Quiz
createQuiz qs = fromList $ zip qs (repeat Nothing)

shuffleQuestions :: (RandomGen gen) => gen -> QuestionList -> QuestionList
shuffleQuestions g [] = []
shuffleQuestions g [x] = [x]
shuffleQuestions g q =
  let shuffled = shuffle' q (length q) g
   in if shuffled /= q then shuffled else shuffleQuestions (fst . split $ g) q