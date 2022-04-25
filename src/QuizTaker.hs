module QuizTaker where

import Control.Concurrent.MState
  ( MState,
    MonadState (get, put),
    modifyM,
  )
import Control.Monad.Cont (MonadIO)
import Data.List.Zipper (Zipper (Zip), cursor, endp, foldlz, fromList, left, replace, right, start, toList)
import Data.Maybe (fromMaybe)
import qualified Quiz as Q

type QuizTaker m a = MState Q.Quiz m a

next :: (MonadIO m) => QuizTaker m ()
next = modifyM (\s -> ((), Q.next s))

prev :: (MonadIO m) => QuizTaker m ()
prev = modifyM (\s -> ((), Q.prev s))

curr :: (MonadIO m) => QuizTaker m Q.Question
curr = fst <$> currAnswer

currAnswer :: (MonadIO m) => QuizTaker m Q.AnsweredQuestion
currAnswer = Q.currAnswer <$> get

hasNext, hasPrev :: (MonadIO m) => QuizTaker m Bool
hasNext = Q.hasNext <$> get
hasPrev = Q.hasPrev <$> get

answerCurr :: (MonadIO m) => Int -> QuizTaker m (Maybe (Int, Int))
answerCurr n = do
  s <- get
  x <- curr
  if Q.validAnswer n x
    then do
      put $ replace (x, Just n) s
      return . Just $ (n, Q.correct x)
    else return Nothing

-- Can easily reduce this down
directionalAnswerCurr :: (MonadIO m) => Q.Direction -> QuizTaker m (Int, Int)
directionalAnswerCurr d = do
  (Q.Question _ a ci, maybe) <- currAnswer
  let totalAnswers = length a
  let curr = fromMaybe 0 maybe
  let answer = direction d curr totalAnswers
  answerCurr answer
  return (answer, ci)
  where
    direction Q.Up curr totalAnswers =
      if curr < totalAnswers
        then succ curr
        else 1
    direction Q.Down curr totalAnswers =
      if curr <= 1
        then totalAnswers
        else pred curr