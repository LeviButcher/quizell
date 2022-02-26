module MainHelpers
  ( getQuizFile,
    getParsedQuiz,
    randomizeQuiz,
    trimQuiz,
    normalApp,
    transformFinishedQuiz,
    tuiApp,
  )
where

import Brick (defaultMain)
import Control.Arrow (left)
import Control.Exception (try)
import Data.Maybe (catMaybes)
import Data.Time (diffUTCTime, getCurrentTime)
import qualified Lib as L
import qualified QuizParser as QP
import System.Random (newStdGen)
import TUI (QuizState (..), quizApp, startState)
import Text.Printf

getQuizFile :: String -> IO (Either String String)
getQuizFile quizPath = do
  readResult <- try (readFile quizPath) :: IO (Either IOError String)
  return $ left (const $ "File entered does not exist: " ++ quizPath) readResult

getParsedQuiz :: Either String String -> IO (Either String L.QuizQuestions)
getParsedQuiz x = return $ do
  r <- x
  left (const "Failed to parse file correctly\nPlease try again\n") $ QP.parseQuiz . QP.cleanQuizString $ r

randomizeQuiz :: Either String L.QuizQuestions -> IO (Either String L.QuizQuestions)
randomizeQuiz (Right quiz) = do
  rng <- newStdGen
  return . Right $ L.shuffleQuiz rng quiz
randomizeQuiz l = return l

trimQuiz :: Int -> Either String L.QuizQuestions -> Either String L.QuizQuestions
trimQuiz _ (Left l) = Left l
trimQuiz n (Right q)
  | n < 0 = Left $ show n ++ " is a invalid number of questions"
  | n == 0 = Right q
  | n <= length q = Right $ take n q
  | otherwise = Left $ "Quiz only has " ++ show n ++ "Questions"

normalApp :: L.QuizQuestions -> IO ()
normalApp quiz = do
  quizStart <- getCurrentTime
  finishedQuiz <- L.takeQuiz putStr getLine quiz
  quizEnd <- getCurrentTime
  L.presentResults putStrLn finishedQuiz
  let elapsedTime = diffUTCTime quizEnd quizStart
  printf "Total Time: %s" (show elapsedTime)

transformFinishedQuiz :: (Maybe Int, L.QuizQuestion) -> Maybe (Int, L.QuizQuestion)
transformFinishedQuiz (Nothing, _) = Nothing
transformFinishedQuiz (Just x, y) = Just (succ x, y)

tuiApp :: L.QuizQuestions -> IO ()
tuiApp quiz = do
  s <- startState quiz
  finalState <- defaultMain quizApp s
  let maybeAnswers = _answeredQuiz finalState
  let answers = catMaybes $ transformFinishedQuiz <$> maybeAnswers
  return ()