{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Model where

import Data.Maybe (fromMaybe)
import Data.Time.Clock
import qualified Quiz as Q
import qualified QuizCLI as CLI
import qualified QuizResults as R

-- Should change this stuff to Maybe Types
data Model = Model
  { quiz :: Q.Quiz,
    taker :: Maybe String,
    startTime :: UTCTime,
    state :: State,
    pastResults :: [R.QuizResults],
    allotedTime :: Int
  }
  deriving (Eq, Show)

data State
  = Home -- Show Main Screen of App
  | QuizConfig -- Show Form Dialog for quiz
  | RunningQuiz -- Show Taking Quiz Screen
  | Finished -- Show Results Screen
  | PastResults
  | UserForm
  deriving (Eq, Show)

-- | Sum type for application events
data Action
  = Init -- Starting Action of App
  | QuizFormStart -- Change model state to UploadQuiz
  | QuizFormSubmit -- Parse out inputs from UploadQuizForm
  | SetQuizConfig Int Q.QuestionList -- Parsed info from Quiz Form
  | Next -- Change quiz to next question
  | Answer Int -- Sets answer for current question
  | GetPastResults -- Load the past results and passes them to the ShowPastResults Action
  | ShowPastResults [R.QuizResults] -- Sets State to Past Results and sets past results
  | Finish -- Change Model State to Finished
  | ShowHome -- Change Model State to Home
  | ShowUserForm -- Change Model State to UserForm
  | SubmitUserForm -- User form was submitted
  | SetUserName String -- update Model with User name
  deriving (Show, Eq)