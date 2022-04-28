{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Model where

import Data.Maybe (fromMaybe)
import Data.Time.Clock
import qualified Quiz as Q
import qualified QuizCLI as CLI
import qualified QuizResults as R
import Data.Time.Clock

-- Should change this stuff to Maybe Types
data Model = Model
  { quiz :: Q.Quiz,
    taker :: Maybe String,
    startTime :: UTCTime,
    state :: State,
    pastResults :: [R.QuizResults],
    allotedTime :: Int,
    formError :: Maybe String
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
  | SetQuizConfig Int Q.Quiz -- Parsed info from Quiz Form
  | Next -- Change quiz to next question
  | Answer Int -- Sets answer for current question
  | GetPastResults -- Load the past results and passes them to the ShowPastResults Action
  | ShowPastResults [R.QuizResults] -- Sets State to Past Results and sets past results
  | Finish -- Change Model State to Finished
  | ShowHome -- Change Model State to Home
  | ShowUserForm -- Change Model State to UserForm
  | SubmitUserForm -- User form was submitted
  | SetUserName String -- update Model with User name
  | SetFormError String -- updates Model with form Error (Doesn't change state)
  deriving (Show, Eq)


createModel :: Q.Quiz -> UTCTime -> Model
createModel q t = Model { 
    quiz=q,
    taker=Nothing,
    startTime=t,
    state=Home, 
    pastResults=[],
    allotedTime=0,
    formError = Nothing
  }

getModelResults :: Model -> Maybe R.QuizResults
getModelResults Model{quiz,taker,startTime} = R.getResults <$> taker <*> Just "?" <*> 
  Just quiz <*> Just startTime <*> Just startTime <*> Just allotedTime
  where allotedTime = 0