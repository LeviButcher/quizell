module QuizCLI where

import Control.Applicative ((<|>))
import Control.Arrow (left)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MState (MState, forkM, forkM_, killMState, waitM)
import Control.Exception (try)
import Control.Monad.Cont (MonadIO, MonadTrans (lift))
import Data.Bifunctor (Bifunctor (first))
import Data.Either (fromRight)
import Data.Functor ((<&>))
import QuestionParser as QP (parseQuestions)
import Quiz (Question (Question, question), QuizTaker, answerCurr, curr, hasNext, next)
import qualified Quiz as Q
import qualified QuizResults as QR
import System.Random (newStdGen)
import Utils (Log (readLog), allTrue, errorText, getNumberOrDefault, goodNewsText, joinDelim, numberStrings, resetScreen)

printQuestion :: Question -> String
printQuestion (Question q a _) = unlines [q, "", numbAnswer]
  where
    numbAnswer = numberStrings a

userAnswerCurr :: QuizTaker IO Bool
userAnswerCurr = do
  q <- curr
  lift $ putStrLn (printQuestion q)
  lift $ putStrLn "Enter # of answer:"
  input <- lift $ getNumberOrDefault 0
  (ai, ci) <- answerCurr input

  lift $ putStrLn ""
  if ai == ci
    then do
      lift . goodNewsText putStrLn $ "You Answered Correctly!"
    else do
      lift . errorText putStrLn $ "You Answered Incorrectly!"
      lift . putStrLn $ "Correct Answer was #" ++ show ci
  lift $ putStrLn ""

  lift $ putStrLn "Press enter to continue..."
  lift getLine
  return (ai == ci)

takeQuiz :: QuizTaker IO ()
takeQuiz = do
  lift resetScreen
  userAnswerCurr
  b <- hasNext
  if not b
    then return ()
    else do
      next
      takeQuiz

getQuizFile :: String -> IO (Either String String)
getQuizFile quizPath = do
  readResult <- try (readFile quizPath) :: IO (Either IOError String)
  return $ left (const $ "File entered does not exist: " ++ quizPath) readResult

getParsedQuiz :: String -> Either String Q.QuestionList
getParsedQuiz = first (const "Failed to parse file correctly\nPlease try again\n") . QP.parseQuestions

randomizeQuiz :: Q.QuestionList -> IO Q.QuestionList
randomizeQuiz qList = do
  rng <- newStdGen
  return $ Q.shuffleQuestions rng qList

trimQuiz :: Int -> Q.QuestionList -> Either String Q.QuestionList
trimQuiz n q
  | n < 0 = Left $ show n ++ " is a invalid number of questions"
  | n == 0 = Right q
  | n <= qLength = Right $ take n q
  | otherwise = Left $ "Quiz only has " ++ show qLength ++ " Questions"
  where
    qLength = length q

printUserLogs :: IO String -> IO ()
printUserLogs getName = do
  user <- getName
  logs <- readLog
  let userLogs = filter ((user ==) . QR.taker) logs
  putStrLn $ QR.showResults userLogs

getQuestionList :: FilePath -> Int -> IO (Either String Q.QuestionList)
getQuestionList file n = do
  txt <- getQuizFile file
  randomQ <- sequence $ txt >>= getParsedQuiz <&> randomizeQuiz
  return $ randomQ >>= trimQuiz n

timer :: Int -> QuizTaker IO ()
timer n = do
  lift (threadDelay n)
  lift resetScreen
  lift $ errorText putStrLn "You have run out of time!"
  return ()

timedTakeQuiz :: Int -> QuizTaker IO ()
timedTakeQuiz n =
  do
    quizId <- forkM takeQuiz
    timerId <- forkM (timer n)

    forkM (waitM quizId >> lift (killThread timerId))
    forkM (waitM timerId >> lift (killThread quizId))

    return ()