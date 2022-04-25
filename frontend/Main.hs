-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

-- | Miso framework import
import Miso hiding (asyncCallback)
import QuestionParser
import qualified Quiz as Q
import qualified QuizCLI as CLI
import qualified QuizResults as R
import Miso.String (ms)
import Data.Maybe (fromMaybe)

-- MVC TODO
-- [] Calculate and Show Results
-- [] Handle process of upload quiz file
-- [] Make it pretty with CSS
-- [] Store Result in LocalStorage As Log

-- Extra TODO
-- [] Time Limit on quiz
-- [] Have Default Quizzes Available to take

-- NOTE: Doing the Time Limit would make it easier to do the Results



data Model = Model {
  quiz :: Q.Quiz,
  finished :: Bool
} deriving (Eq, Show)

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
  

-- | Sum type for application events
data Action = Prompt | Next | Answer Int | Finish
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = do
  let quiz = Q.createQuiz fakeQuestionList
  startApp 
    App {
      initialAction =  Prompt,
      model = Model{quiz=quiz, finished=False},
      update = updateModel,
      view   = layout viewModel,
      events = defaultEvents,
      subs   = [],
      mountPoint = Nothing,
      logLevel = DebugPrerender
    }

updateModel :: Action -> Model -> Effect Action Model
updateModel Prompt m = noEff m
updateModel Next m = noEff $ m {quiz=Q.next (quiz m)} -- TODO: How to handle when their is no next -- USE LENS --
updateModel Finish m = noEff $ m {finished=True}
updateModel (Answer a) m = answerCurr a m

answerCurr :: Int -> Model -> Effect Action Model
answerCurr i m = noEff $ m {quiz = newQuiz}
  where q = quiz m
        newQuiz = fromMaybe q (Q.answerCurr i q)



-- Vomiting that theres no way to put the style sheet in a head tag
-- TODO: Figure out a way to keep the css and index.html beside each other
layout :: (Model -> View Action) -> Model -> View Action 
layout f m = main_ [] [styleSheet, va]
  where va = f m
        styleSheet = link_ [rel_ "stylesheet", href_ "file:///home/levi/Projects/quizell/frontend/quizell.css"]


viewModel :: Model -> View Action
viewModel (Model q True) = showResults q
viewModel m = currentQuestion m

showResults :: Q.Quiz -> View Action
showResults q = div_ [] [text "YAY you finished"]


currentQuestion :: Model -> View Action
currentQuestion (Model m _) = 
  let (Q.Question quest _ _, _) = Q.currAnswer m
      nextButton = button_ [onClick Next, type_ "button"] [text "Next"]
      finishButton = button_ [type_ "submit"] [text "Finish"] -- Maybe should show this always?
  in form_ [name_ "Quiz Question", onSubmit Finish] [
      header_ [] [
        h2_ [] [text (ms quest)]
      ],
      answers (Q.currAnswer m),
      footer_ [] [
        nextButton,
        finishButton
      ]
    ]

answers :: Q.AnsweredQuestion -> View Action
answers (Q.Question _ answers _, ans) = fieldset_ [] (legend:items)
    where 
      item :: (Int, String) -> View Action -- Vomiting over having to put inputs inside labels :/
      item (i, x) = label_ [] [ 
          text (ms x),
          input_ [ type_ "checkbox", value_ (ms i), checked_ (isChecked ans i), onClick (Answer i)]
        ]
      isChecked :: Maybe Int -> Int -> Bool
      isChecked Nothing = const False
      isChecked (Just a) = (== a)
      legend = legend_ [] [text "Select a Answer"]
      items = item <$> zip [1 ..] answers