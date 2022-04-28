module QuizCLI where

import Control.Applicative ((<|>))
import Control.Arrow (left)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MState (MState, MonadState (state), forkM, forkM_, killMState, waitM)
import Control.Exception (try)
import Control.Monad.Cont (MonadIO, MonadTrans (lift))
import Data.Bifunctor (Bifunctor (first))
import Data.Either (fromRight)
import Data.Functor ((<&>))
import QuestionParser as QP (parseQuestions)
import Quiz (Question (..))
import qualified Quiz as Q
import qualified QuizResults as QR
import qualified QuizTaker as QT
import System.Random (newStdGen)
import Utils (Log (readLog), allTrue, errorText, getNumberOrDefault, goodNewsText, joinDelim, numberStrings, resetScreen)
import Control.Monad.Except

printQuestion :: Question -> String
printQuestion (Question q a _) = unlines [q, "", numbAnswer]
  where
    numbAnswer = numberStrings a

userAnswerCurr :: QT.QuizTaker IO Bool
userAnswerCurr = do
  q <- QT.curr
  lift $ putStrLn (printQuestion q)
  lift $ putStrLn "Enter # of answer:"
  input <- lift $ getNumberOrDefault 0
  possibleAns <- QT.answerCurr input

  case possibleAns of
    Nothing -> (lift . putStrLn $ "Invalid Answer, try again") >> userAnswerCurr
    Just (ai, ci) -> do
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

takeQuiz :: QT.QuizTaker IO ()
takeQuiz = do
  lift resetScreen
  userAnswerCurr
  b <- QT.hasNext
  if not b
    then return ()
    else do
      QT.next
      takeQuiz

-- Has User enter filePath and reads it
getQuestionFile :: String -> IO (Either String String)
getQuestionFile quizPath = do
  readResult <- try (readFile quizPath) :: IO (Either IOError String)
  return $ left (const $ "File entered does not exist: " ++ quizPath) readResult

-- Takes in the FileString
getParsedQuestions :: String -> Either String Q.QuestionList
getParsedQuestions = first (const "Failed to parse file correctly\nPlease try again\n") . 
  QP.parseQuestions

printUserLogs :: (String -> IO String) -> IO String -> IO ()
printUserLogs getStorage getName = do
  user <- getName
  logs <- readLog getStorage
  let userLogs = filter ((user ==) . QR.taker) logs
  putStrLn $ QR.showResults userLogs

getQuestionList :: FilePath -> IO (Either String Q.QuestionList)
getQuestionList file = runExceptT $ do
  txt <- ExceptT $ getQuestionFile file
  liftEither $ getParsedQuestions txt

timer :: Int -> QT.QuizTaker IO ()
timer n = do
  lift (threadDelay n)
  lift resetScreen
  lift $ errorText putStrLn "You have run out of time!"
  return ()

timedTakeQuiz :: Int -> QT.QuizTaker IO ()
timedTakeQuiz n =
  do
    quizId <- forkM takeQuiz
    timerId <- forkM (timer n)

    forkM (waitM quizId >> lift (killThread timerId))
    forkM (waitM timerId >> lift (killThread quizId))

    return ()