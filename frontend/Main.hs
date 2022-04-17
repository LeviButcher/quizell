-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

-- | Miso framework import
import Miso hiding (asyncCallback)
import Miso.String
import GHCJS.Foreign.Callback
import GHCJS.Types
import Control.Concurrent.MVar

-- TODO
-- [] Design out Application State and Action

-- APPSTATE = NoQuiz | Quiz x | Finished Quiz x
-- Action = Init | ShowForm | StartQuiz | SubmitQuiz


data Model = Start | OpenedForm | Quiz MisoString | FinishedQuiz MisoString
  deriving (Show, Eq)


-- | Sum type for application events
data Action = Init | OpenForm | SubmitForm | ParseQuiz MisoString | StartQuiz | SubmitQuiz
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: JSM ()
main = startApp App {..}
  where
    initialAction =  Init 
    model = Start 
    update = updateModel
    view   = viewModel
    events = defaultEvents
    subs   = []
    mountPoint = Nothing
    logLevel = Off  

updateModel :: Action -> Model -> Effect Action Model
updateModel SubmitForm = processFile
updateModel OpenForm = const $ noEff (OpenedForm)
updateModel (ParseQuiz c) = const $ noEff (Quiz c)
updateModel a = noEff


processFile :: Model -> Effect Action Model
processFile m = process #> m
  where 
    process = do
      fileReaderInput <- getElementById "fileReader"
      file <- getFile fileReaderInput
      reader <- newReader
      mvar <- newEmptyMVar
      setOnLoad reader =<< do
        asyncCallback $ do
          r <- getResult reader
          putMVar mvar r
      readText reader file
      ParseQuiz <$> readMVar mvar


-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m = layout $ selectView m
  where
    selectView Start = homeScreen
    selectView OpenedForm = viewForm
    selectView (Quiz x) = div_ [] [text x]

homeScreen :: View Action
homeScreen = div_ [] [
  button_ [onClick OpenForm] [text "Add Quiz"]
  ]

layout :: View Action -> View Action
layout children = main_ [] [header, children]

-- Not correct way to link css
header :: View Action
header = header_ [] [
  link_ [href_ "./quizell.css"], 
  h1_ [] ["Quizell Web App"]
  ]

viewForm :: View Action
viewForm = form
  where 
    form = form_ [id_ "quizForm", onSubmit SubmitForm] [
      label_ [for_ "file"] [text "Select File"], 
      input_ [type_ "file", name_ "file", id_ "fileReader"],
      button_ [type_ "submit"] [text "Submit"]
      ]



-- I hate the below code for reading file
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