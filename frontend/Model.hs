{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Model where

import Data.Maybe (fromMaybe)
import Data.Time.Clock
import qualified Quiz as Q
import qualified QuizCLI as CLI
import qualified QuizResults as R

data Model = Model
  { quiz :: Q.Quiz,
    taker :: String,
    startTime :: UTCTime,
    state :: State
  }
  deriving (Eq, Show)

data State
  = Home -- Show Main Screen of App
  | UploadQuestions -- Show Form Dialog for quiz
  | RunningQuiz -- Show Taking Quiz Screen
  | Finished -- Show Results Screen
  deriving (Eq, Show)

-- | Sum type for application events
data Action
  = Init -- Starting Action of App
  | QuizFormStart -- Change model state to UploadQuiz
  | QuizFormSubmit -- Parse out inputs from UploadQuizForm
  | QuizForm String Q.QuestionList -- Parsed info from Quiz Form
  | Next -- Change quiz to next question
  | Answer Int -- Sets answer for current question
  | Finish -- Change Model State to Finished
  | Reset -- Change Model State to Home
  deriving (Show, Eq)