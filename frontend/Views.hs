{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Views where

import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Miso hiding (asyncCallback)
import Miso.String (ToMisoString, ms)
import Model
import QuestionParser
import qualified Quiz as Q
import qualified QuizCLI as CLI
import qualified QuizResults as R

-- Vomiting that theres no way to put the style sheet in a head tag
-- TODO: Figure out a way to keep the css and index.html beside each other
layout :: (Model -> View Action) -> Model -> View Action
layout f m = main_ [] [styleSheet, title, va]
  where
    va = f m
    styleSheet = link_ [rel_ "stylesheet", href_ "file:///home/levi/Projects/quizell/frontend/quizell.css"]
    title = h1_ [] [text "Quizell! - the haskell quiz taker"]

viewModel :: Model -> View Action
viewModel m@Model {state} = case state of
  Home -> viewHome m
  RunningQuiz -> currentQuestion m
  Finished -> viewFinishScreen m
  UploadQuestions -> viewUploadQuestion m

viewHome :: Model -> View Action
viewHome m =
  div_
    []
    [ text "Welcome HOME",
      button_ [onClick QuizFormStart] [text "Select a Quiz File"]
    ]

-- Maybe Wrap in Dialog
viewUploadQuestion :: Model -> View Action
viewUploadQuestion m =
  form_
    [id_ "quizForm", onSubmit QuizFormSubmit]
    [ header_ [] [h2_ [] [text "Setup Quiz"]],
      div_
        []
        [ label_ [for_ "name"] [text "Name"],
          input_ [type_ "text", name_ "name", id_ "name"],
          label_ [for_ "questions"] [text "File"],
          input_ [type_ "File", name_ "questions", id_ "questions"]
        ],
      footer_
        []
        [button_ [type_ "submit"] [text "Submit"]]
    ]

viewFinishScreen :: Model -> View Action
viewFinishScreen Model {quiz, taker, startTime} =
  div_
    []
    [ result res,
      resetButton
    ]
  where
    res = R.getResults taker "?" quiz startTime startTime allotedTime
    resetButton = button_ [onClick Reset] [text "Go Back To Home"]
    allotedTime = 0

result :: R.QuizResults -> View Action
result R.QuizResults {total, correct, taker, testFile, startTime, endTime, allotedTime} =
  article_
    []
    [ header_ [] [h3_ [] [ezText $ "Results for: " ++ taker]],
      div_
        []
        [ ul_
            []
            [ li_ [] [ezText $ "Test File: " ++ testFile],
              li_ [] [ezText $ "Alloted Time: " ++ show allotedTime],
              li_ [] [ezText $ "Total Correct: " ++ show correct],
              li_ [] [ezText $ "Total Questions: " ++ show total],
              li_ [] [ezText $ "Start Time:" ++ show startTime],
              li_ [] [ezText $ "End Time:" ++ show endTime]
            ]
        ]
    ]

currentQuestion :: Model -> View Action
currentQuestion Model {quiz} =
  let (Q.Question quest _ _, _) = Q.currAnswer quiz
      nextButton = button_ [onClick Next, type_ "button"] [text "Next"]
      finishButton = button_ [type_ "submit"] [text "Finish"] -- Maybe should show this always?
   in form_
        [name_ "Quiz Question", onSubmit Finish]
        [ header_
            []
            [ h2_ [] [text (ms quest)]
            ],
          answers (Q.currAnswer quiz),
          footer_
            []
            [ nextButton,
              finishButton
            ]
        ]

answers :: Q.AnsweredQuestion -> View Action
answers (Q.Question _ answers _, ans) = fieldset_ [] (legend : items)
  where
    item :: (Int, String) -> View Action -- Vomiting over having to put inputs inside labels :/
    item (i, x) =
      label_
        []
        [ text (ms x),
          input_ [type_ "checkbox", value_ (ms i), checked_ (isChecked ans i), onClick (Answer i)]
        ]
    isChecked :: Maybe Int -> Int -> Bool
    isChecked Nothing = const False
    isChecked (Just a) = (== a)
    legend = legend_ [] [text "Select a Answer"]
    items = item <$> zip [1 ..] answers

ezText :: (ToMisoString s) => s -> View Action
ezText = text . ms