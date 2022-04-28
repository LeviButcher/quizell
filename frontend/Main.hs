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
import Miso.String hiding (take)
import qualified ResultStore as RS
import System.Random (newStdGen)
import Control.Monad.Except

-- MVC TODO
-- [x] Calculate and Show Results
-- [x] Handle process of upload quiz file
-- [x] Make it pretty with CSS
-- [x] Setup Deployment

-- Extra TODO
-- [x] Store Result in LocalStorage As Log
-- [x] Show Results on All results page
-- [x] Have user config form then have all Results only show that users results
-- [x] Improve CSS within forms
-- [x] Improve showing errors on forms and handling them
-- [] Improve Backend QuestionList to Quiz creation
-- [] Time Limit on quiz
-- [] Have Default Quizzes Available to take
-- [] Show Correct Answers after quiz is finished

-- | Entry point for a miso application
main :: IO ()
main = do
  let quiz = Q.createQuiz []
  time <- getCurrentTime
  startApp 
    App {
      initialAction =  Init,
      model = createModel quiz time,
      update = updateModel,
      view   = layout viewModel,
      events = defaultEvents,
      subs   = [],
      mountPoint = Nothing,
      logLevel = DebugPrerender
    }

updateModel :: Action -> Model -> Effect Action Model
updateModel Init m = noEff m
updateModel ShowHome m = noEff $ m{state=Home}

-- Quiz Actions
updateModel Next m@Model{quiz} = noEff $ m {quiz=if Q.hasNext quiz then Q.next quiz else quiz} 
updateModel Finish m = storeResultsInStorage m *> noEff (m {state=Finished})
updateModel (Answer a) m = answerCurr a m

-- Quiz Config Form
updateModel QuizFormStart m = noEff $ m {state = QuizConfig}
updateModel QuizFormSubmit m = handleQuizFormSubmit m
updateModel (SetQuizConfig aTime q) m = 
    noEff $ m {quiz=q, allotedTime=aTime, state = RunningQuiz}

-- Past Results
updateModel GetPastResults m = m <# (ShowPastResults <$> RS.getPastResults m)
updateModel (ShowPastResults results) m = noEff $ m {state = PastResults, pastResults=results}

-- User Config Form
updateModel ShowUserForm m = noEff $ m {state = UserForm}
updateModel SubmitUserForm m = handleUserFormSubmit m
updateModel (SetUserName name) m = m {taker=Just name} <# pure ShowHome

-- Sets Form Information
updateModel (SetFormError e) m = noEff $ m {formError=Just e}


handleUserFormSubmit :: Model -> Effect Action Model
handleUserFormSubmit m = m <# do
  -- Ignores failure to get name
  name <- getInputValue "name"
  return $ SetUserName (fromMisoString name)


storeResultsInStorage :: Model -> Effect Action Model
storeResultsInStorage m = m <# do
  RS.storeResults m
  return Init
  

answerCurr :: Int -> Model -> Effect Action Model
answerCurr i m = noEff $ m {quiz = newQuiz}
  where q = quiz m
        newQuiz = fromMaybe q (Q.answerCurr i q)

handleQuizFormSubmit :: Model -> Effect Action Model
handleQuizFormSubmit m = m <# do
  quizFormEither <- runExceptT getQuizFormData
  case quizFormEither of
    Left err -> consoleLog (ms . show $ err) >> (return $ SetFormError err)
    Right act -> return act


getQuizFormData :: ExceptT String IO Action 
getQuizFormData = do
  time <- ExceptT (fromMisoStringEither <$> getInputValue "allotedTime")
  questionCount <- ExceptT (fromMisoStringEither <$> getInputValue "questions")
  file <- liftIO readFileFromForm
  fileString <- liftEither $ fromMisoStringEither file
  questions <- withExceptT 
    (const "Failed to parse file - Make sure a correctly formatted quiz file is selected") 
    (liftEither $ parseQuestions fileString)
  rng <- liftIO newStdGen
  quiz <- liftEither $ Q.createShuffledQuizToLength rng questionCount questions
  return $ SetQuizConfig time quiz


readFileFromForm :: IO MisoString
readFileFromForm = do
  fileReaderInput <- getElementById "questionsFile"
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