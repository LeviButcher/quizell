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

type QuizTaker m a = MState Quiz m a

next :: (MonadIO m) => QuizTaker m ()
next = modifyM (\s -> ((), right s))

prev :: (MonadIO m) => QuizTaker m ()
prev = modifyM (\s -> ((), left s))

curr :: (MonadIO m) => QuizTaker m Question
curr = fst <$> currAnswer

currAnswer :: (MonadIO m) => QuizTaker m AnsweredQuestion
currAnswer = cursor <$> get

hasNext :: (MonadIO m) => QuizTaker m Bool
hasNext = do
  not . endp . Z.right <$> get

hasPrev :: (MonadIO m) => QuizTaker m Bool
hasPrev = do
  not . Z.beginp . Z.left <$> get

validAnswer :: Int -> Question -> Bool
validAnswer n (Question _ ans _) = n >= 1 && n <= totalAnswers
  where
    totalAnswers = length ans

answerCurr :: (MonadIO m) => Int -> QuizTaker m (Maybe (Int, Int))
answerCurr n = do
  s <- get
  x <- curr
  if validAnswer n x
    then do
      put $ replace (x, Just n) s
      return . Just $ (n, correct x)
    else return Nothing

-- Can easily reduce this down
directionalAnswerCurr :: (MonadIO m) => Direction -> QuizTaker m (Int, Int)
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