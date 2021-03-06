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
import Control.Applicative
import Control.Concurrent

-- Should change this stuff to Maybe Types
data Model = Model
  { taker :: Maybe String,
    state :: State,
    pastResults :: [R.QuizResults],
    formError :: Maybe String,
    quizConfig :: Maybe QuizConfig,
    forkList :: [ThreadId]
  } deriving (Eq, Show)

data QuizConfig = QuizConfig {
  quiz :: Q.Quiz,
  startTime :: UTCTime,
  allotedTime :: Int,
  testFile :: String,
  endTime :: Maybe UTCTime
} deriving (Eq, Show)

data State
  = Home -- Show Main Screen of App
  | QuizConfigForm -- Show Form Dialog for quiz
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
  | SetQuizConfig QuizConfig -- Parsed info from Quiz Form
  | Next -- Change quiz to next question
  | Answer Int -- Sets answer for current question
  | GetPastResults -- Load the past results and passes them to the ShowPastResults Action
  | ShowPastResults [R.QuizResults] -- Sets State to Past Results and sets past results
  | FinishedQuiz -- Kick off effect to to get finishedTime
  | SetEndTime UTCTime -- Set the quizConfigs endtime
  | ShowQuizResult -- Update the model to Finished
  | ShowHome -- Change Model State to Home
  | ShowUserForm -- Change Model State to UserForm
  | SubmitUserForm -- User form was submitted
  | SetUserName String -- update Model with User name
  | SetFormError String -- updates Model with form Error (Doesn't change state)
  | StartTimer -- Start the timer to close the quiz after alloted time
  | AddForkId ThreadId -- Adds a forkID to forkList
  | EndForks -- Ends all forks in forkList
  deriving (Eq, Show)


createDefaultModel :: IO Model
createDefaultModel = return $ Model { 
    taker=Nothing,
    state=Home, 
    pastResults=[],
    formError = Nothing,
    quizConfig=Nothing,
    forkList=[]
  }

getModelResults :: Model -> Maybe R.QuizResults
getModelResults Model{taker,quizConfig} = do
  QuizConfig{quiz,startTime,allotedTime,testFile, endTime} <- quizConfig
  name <- taker <|> (Just "No name")
  end <- endTime
  return $ R.getResults name testFile quiz startTime end allotedTime