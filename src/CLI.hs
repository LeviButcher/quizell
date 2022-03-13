{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module CLI where

import Control.Exception (try)
import Control.Monad (void)
import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.List.Zipper (toList)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import qualified Quiz as Q
import QuizResults (QuizResults (correct, taker, testFile, total), getResults)
import System.Random (RandomGen (split))
import System.Random.Shuffle (shuffle, shuffle')
import Text.Printf (printf)
import Text.Read (readMaybe)
import Utils (allTrue, joinDelim)

type GetLine m = m String

type PutStr m = (String -> m ())

type GetUserName m = m String

numberAnswers :: [String] -> String
numberAnswers s = unlines $ (\(a, b) -> concat [show a, ") ", b]) <$> zip [1 ..] s

outputQuestion :: Monad m => PutStr m -> Q.Question -> m ()
outputQuestion putStr (Q.Question question answers _) = do
  let putStrLn = (\x -> putStr $ x ++ "\n")
      numberedAnswers = numberAnswers answers
  putStr "\nQuestion: "
  putStrLn question
  putStrLn "\nPossible Choices: "
  putStrLn numberedAnswers

outputCorrectAnswerInfo :: (Monad m, Q.Quiz q) => PutStr m -> GetLine m -> q -> m ()
outputCorrectAnswerInfo putStr getLine q =
  do
    let x@(Q.Question _ _ ci, _) = Q.currAnswer q
    if Q.isCurrCorrect q
      then putStr "You answered correctly!!!"
      else
        putStr "You answered incorrectly"
          >> putStr ("#" ++ show ci ++ " was the correct answer\n")
    >> putStr "Press Enter to continue"
    >> getLine
    >> return ()

userAnswerQuestion :: (Monad m, Q.Quiz q) => PutStr m -> GetLine m -> q -> m (Either Q.QuizError q)
userAnswerQuestion putStr getLine quiz = do
  let x@(Q.Question question answers correctI) = Q.curr quiz
  let putStrLn = (\x -> putStr $ x ++ "\n")
  outputQuestion putStr x
  putStrLn "Enter # of answer:"
  guess <- fromMaybe 0 . readMaybe <$> getLine
  return $ Q.answerCurr quiz guess

takeQuiz :: (Monad m, Q.Quiz q) => GetUserName m -> PutStr m -> GetLine m -> String -> q -> m QuizResults
takeQuiz getUser putStr getLine file q = do
  let putStrLn = (\x -> putStr $ x ++ "\n")
  res <- userAnswerQuestion putStr getLine q

  case res of
    Left err -> case err of
      Q.InvalidAnswer -> putStrLn "Invalid Answer\nTry Again\n\n" >> takeQuiz getUser putStr getLine file q
    Right zip -> do
      outputCorrectAnswerInfo putStrLn getLine zip -- Output answer info before moving to next questions
      if Q.hasNext q
        then takeQuiz getUser putStr getLine file (Q.next zip)
        else getResults <$> getUser <*> pure file <*> pure q

presentResults :: Monad m => PutStr m -> QuizResults -> m ()
presentResults putStrLn qr = do
  let totalQ = total qr
      correctQ = correct qr
      percentC = fromIntegral correctQ / fromIntegral totalQ * 100
      file = testFile qr
      user = taker qr
  putStrLn $ "Results for: " ++ user
  putStrLn $ "Total Correct: " ++ show correctQ
  putStrLn $ "Total Questions: " ++ show totalQ
  putStrLn $ "Percentage Correct: " ++ show percentC ++ "%"
  putStrLn $ "Test File: " ++ file
