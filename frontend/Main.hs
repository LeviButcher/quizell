-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

-- | Miso framework import
import Miso hiding (asyncCallback)
import QuestionParser as QP
import qualified Quiz as Q
import qualified QuizCLI as CLI
import qualified QuizResults as R
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Model
import Views
import Control.Concurrent.MVar
import GHCJS.Foreign.Callback
import GHCJS.Types
import Miso.String

-- MVC TODO
-- [x] Calculate and Show Results
-- [x] Handle process of upload quiz file
-- [] Make it pretty with CSS

-- Extra TODO
-- [] Store Result in LocalStorage As Log
-- [] Time Limit on quiz
-- [] Have Default Quizzes Available to take

-- NOTE: Doing the Time Limit would make it easier to do the Results

fakeQuestionList :: Q.QuestionList
fakeQuestionList = [ 
  Q.Question {
      Q.question = "What is my favorite color",
      Q.answers = ["Blue", "Red", "Green", "WHAT I DONT KNOW THAT"],
      Q.correct = 1 
    },
  Q.Question {
      Q.question = "How are you",
      Q.answers = ["Good", "Bad", "IDK", "WHAT I DONT KNOW THAT"],
      Q.correct = 1 
    },
  Q.Question
    { Q.question = "Best Zelda Game",
      Q.answers = ["Zelda 2", "LTTP", "Every One"],
      Q.correct = 2
    }
  ]


-- | Entry point for a miso application
main :: IO ()
main = do
  let quiz = Q.createQuiz fakeQuestionList
      taker = "Levi"
  time <- getCurrentTime

  startApp 
    App {
      initialAction =  Init,
      model = createModel quiz time taker,
      update = updateModel,
      view   = layout viewModel,
      events = defaultEvents,
      subs   = [],
      mountPoint = Nothing,
      logLevel = DebugPrerender
    }

createModel :: Q.Quiz -> UTCTime -> String -> Model
createModel q t s = Model q s t Home

updateModel :: Action -> Model -> Effect Action Model
updateModel Init m = noEff m
-- TODO: How to handle when their is no next -- USE LENS --
updateModel Next m = noEff $ m {quiz=Q.next (quiz m)} 
updateModel Finish m = noEff $ m {state=Finished}
updateModel (Answer a) m = answerCurr a m
updateModel Reset m = noEff $ m{state=Home}
updateModel QuizFormStart m = noEff $ m {state = UploadQuestions}
updateModel QuizFormSubmit m = handleQuizFormSubmit m
updateModel (QuizForm name qList) m = 
    noEff $ m {state = RunningQuiz, taker = name, quiz = Q.createQuiz qList}

answerCurr :: Int -> Model -> Effect Action Model
answerCurr i m = noEff $ m {quiz = newQuiz}
  where q = quiz m
        newQuiz = fromMaybe q (Q.answerCurr i q)

handleQuizFormSubmit :: Model -> Effect Action Model
handleQuizFormSubmit m = m <# do
  name <- getInputValue "name"
  file <- readFileFromForm
  let eitherQuestions = parseQuestions (fromMisoString file)
  case eitherQuestions of 
    Left err -> consoleLog (ms . show $ err) >> return QuizFormStart
    Right questions -> return $ QuizForm (fromMisoString name) questions

readFileFromForm :: IO MisoString
readFileFromForm = do
  fileReaderInput <- getElementById "questions"
  file <- getFile fileReaderInput
  reader <- newReader
  mvar <- newEmptyMVar
  setOnLoad reader =<< do
    asyncCallback $ do
      r <- getResult reader
      putMVar mvar r
  readText reader file
  readMVar mvar

foreign import javascript unsafe "$r = new FileReader();"
  newReader :: IO JSVal

foreign import javascript unsafe "$r = $1.files[0];"
  getFile :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.onload = $2;"
  setOnLoad :: JSVal -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "$r = $1.result;"
  getResult :: JSVal -> IO MisoString

foreign import javascript unsafe "$1.readAsText($2);"
  readText :: JSVal -> JSVal -> IO ()

foreign import javascript unsafe "$r = document.getElementById($1).value;"
  getInputValue :: MisoString -> IO MisoString