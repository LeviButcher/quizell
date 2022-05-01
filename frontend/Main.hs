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
import Control.Applicative
import Control.Monad.Except
import Control.Concurrent
import Data.Time.Format
import Language.Javascript.JSaddle.Value

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
-- [x] Improve Backend QuestionList to Quiz creation
-- [x] Time Limit on quiz
-- [x] Make timers end by ending their threads
-- [x] Show Correct Answers after quiz is finished
-- [] Have Default Quizzes Available to take

-- | Entry point for a miso application
main :: IO ()
main = do
  defaultModel <- createDefaultModel
  startApp 
    App {
      initialAction =  Init,
      model = defaultModel,
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
updateModel Next m@Model{quizConfig} = noEff $ 
  m {quizConfig=updatedQuizConfig quizConfig <|> quizConfig}
  where 
    updatedQuizConfig quizConfig = do
      config@QuizConfig{quiz} <- quizConfig
      Just $ config{quiz=if Q.hasNext quiz then Q.next quiz else quiz} 

updateModel (Answer a) m = answerCurr a m
updateModel FinishedQuiz m = batchEff m [(SetEndTime <$> getCurrentTime), pure EndForks]
updateModel (SetEndTime end) m@Model{quizConfig} = 
  m{quizConfig=newConfig <|> quizConfig} <# (return ShowQuizResult)
  where 
    newConfig = do
      conf <- quizConfig
      -- Adding this here to reset quiz to beginning
      let newQuiz = Q.toBeginning . quiz $ conf
      return $ conf{endTime=Just end, quiz=newQuiz}

updateModel ShowQuizResult m = (storeResultsInStorage m) *> (noEff $ m {state=Finished})

-- Quiz Config Form
updateModel QuizFormStart m = noEff $ m {state = QuizConfigForm}
updateModel QuizFormSubmit m = handleQuizFormSubmit m
updateModel (SetQuizConfig config) m = noEff (m {quizConfig=Just config, state = RunningQuiz} )
updateModel (StartTimer) m = effectSub m startVisualTimer *> (effectSub m (startForcedEndTimer m))

-- Past Results
updateModel GetPastResults m = m <# (ShowPastResults <$> RS.getPastResults m)
updateModel (ShowPastResults results) m = noEff $ m {state = PastResults, pastResults=results}

-- User Config Form
updateModel ShowUserForm m = noEff $ m {state = UserForm}
updateModel SubmitUserForm m = handleUserFormSubmit m
updateModel (SetUserName name) m = m {taker=Just name} <# pure ShowHome

-- Sets Form Information
updateModel (SetFormError e) m = noEff $ m {formError=Just e}
updateModel (AddForkId forkId) m@Model{forkList} = noEff $ m{forkList=forkId:forkList}
updateModel EndForks m = killForks m


killForks :: Model -> Effect Action Model
killForks m@Model{forkList} = m <# do
  traverse killThread forkList
  return Init

startVisualTimer :: Sub Action
startVisualTimer = \sink -> do
  currTime <- getCurrentTime
  forkId <- forkIO (forever $ updateTimer currTime >> threadDelay (sToMicro 1))
  sink $ AddForkId forkId
  where 
    updateTimer startTime = do
      ele <- getElementById "timer" >>= valIsNull
      guard (not ele)
      currTime <- getCurrentTime
      let time = currTime `diffUTCTime` startTime
          -- timeString = formatTime defaultTimeLocale "%h:%m:%s" time
          -- Can't due formatTime because theirs no instance of NominalDiffTime???
      setElementInnerHTML "timer" $ (ms . show) time
      


-- Convert seconds to microseconds
sToMicro :: Int -> Int
sToMicro s = s * 1000000 

startForcedEndTimer :: Model -> Sub Action
startForcedEndTimer Model{quizConfig} = \sink -> do
  case quizConfig of
    Nothing -> sink Init
    Just x -> do
      forkId <- forkIO (sleepingTimer sink x)
      sink $ AddForkId forkId
  where 
    sleepingTimer sink QuizConfig{allotedTime} = do
      if allotedTime > 0 then do
          threadDelay (sToMicro allotedTime)
          -- Check to make sure quiz is still active
          ele <- getElementById "timer" >>= valIsNull
          guard (not ele)
          sink FinishedQuiz 
        else sink Init


handleUserFormSubmit :: Model -> Effect Action Model
handleUserFormSubmit m = m <# do
  name <- getInputValue "name"
  return $ SetUserName (fromMisoString name)


storeResultsInStorage :: Model -> Effect Action Model
storeResultsInStorage m = m <# do
  RS.storeResults m
  return Init
  

answerCurr :: Int -> Model -> Effect Action Model
answerCurr i m@Model{quizConfig} = noEff $ m {quizConfig=newConfig <|> quizConfig}
  where 
    newConfig = do 
      config <- quizConfig
      newQuiz <- Q.answerCurr i (quiz config)
      Just $ config{quiz=newQuiz}

handleQuizFormSubmit :: Model -> Effect Action Model
handleQuizFormSubmit m = m <# do
  quizFormEither <- runExceptT getQuizFormData
  case quizFormEither of
    Left err -> consoleLog (ms . show $ err) >> (return $ SetFormError err)
    Right act -> return act


getQuizFormData :: ExceptT String IO Action 
getQuizFormData = do
  aTime <- ExceptT (fromMisoStringEither <$> getInputValue "allotedTime")
  questionCount <- ExceptT (fromMisoStringEither <$> getInputValue "questions")
  (fileName, fileContent) <- liftIO readFileFromForm
  fileContentString <- liftEither $ fromMisoStringEither fileContent
  fileNameString <- liftEither $ fromMisoStringEither fileName
  questions <- withExceptT 
    (const "Failed to parse file - Make sure a correctly formatted quiz file is selected") 
    (liftEither $ parseQuestions fileContentString)
  rng <- liftIO newStdGen
  quiz <- liftEither $ Q.createShuffledQuizToLength rng questionCount questions
  sTime <- liftIO getCurrentTime
  return $ SetQuizConfig (QuizConfig quiz sTime aTime fileNameString Nothing)


-- Returns back both the fileName and fileContent
readFileFromForm :: IO (MisoString, MisoString)
readFileFromForm = do
  fileReaderInput <- getElementById "questionsFile"
  fileName <- getFileName fileReaderInput
  file <- getFile fileReaderInput
  reader <- newReader
  mvar <- newEmptyMVar
  setOnLoad reader =<< do
    asyncCallback $ do
      r <- getResult reader
      putMVar mvar r
  readText reader file
  fileContent <- readMVar mvar
  return $ (fileName,fileContent)

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

foreign import javascript unsafe "$r = $1.files.item(0).name;"
  getFileName :: JSVal -> IO MisoString

foreign import javascript unsafe "document.getElementById($1).innerHTML = $2"
  setElementInnerHTML :: MisoString -> MisoString -> IO ()

foreign import javascript unsafe "$r = setInterval($1, $2)"
  setInterval :: Callback (IO ()) -> Int -> IO JSVal