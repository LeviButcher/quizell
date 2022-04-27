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
layout :: (Model -> View Action) -> Model -> View Action
layout f m = main_ [] [title, va]
  where
    va = f m
    title = h1_ [] [text "Quizell! - the haskell quiz taker"]

viewModel :: Model -> View Action
viewModel m@Model {state} = case state of
  Home -> viewHome m
  RunningQuiz -> viewTakingQuiz m
  Finished -> viewFinishScreen m
  UploadQuestions -> viewUploadQuestion m
  PastResults -> viewPastResults m

viewHome :: Model -> View Action
viewHome m =
  div_
    []
    [ h2_ [] [text "Welcome - Please Choose a Option"],
      menu_ [class_ "home_menu"] [
        button_ [onClick QuizFormStart] [text "Take a Quiz!"],
        button_ [onClick GetPastResults] [text "View Past Results"]
        ]
    ]

-- Maybe Wrap in Dialog
viewUploadQuestion :: Model -> View Action
viewUploadQuestion m =
  form_
    [id_ "quizForm", onSubmit QuizFormSubmit, class_ "card"]
    [ header_ [] [h2_ [] [text "Setup Quiz"]],
      div_
        []
        [ label_ [for_ "name"] [text "Name"],
          input_ [type_ "text", name_ "name", id_ "name"],
          label_ [for_ "questions"] [text "Upload Question List"],
          input_ [type_ "File", name_ "questions", id_ "questions"]
        ],
      footer_
        []
        [button_ [type_ "submit", class_ "button_dark"] [text "Submit"]]
    ]

viewFinishScreen :: Model -> View Action
viewFinishScreen Model {quiz, taker, startTime} =
  section_
    [class_ "card"]
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
    [class_ "card"]
    [ header_ [] [h3_ [] [text "Results"]],
      div_
        []
        [ ul_
            []
            [ li_ [] [ezText $ "Taker: " ++ taker],
              li_ [] [ezText $ "Test File: " ++ testFile],
              li_ [] [ezText $ "Alloted Time: " ++ show allotedTime],
              li_ [] [ezText $ "Total Correct: " ++ show correct],
              li_ [] [ezText $ "Total Questions: " ++ show total],
              li_ [] [ezText $ "Start Time:" ++ show startTime],
              li_ [] [ezText $ "End Time:" ++ show endTime]
            ]
        ]
    ]


viewTakingQuiz :: Model -> View Action
viewTakingQuiz m = section_ [class_ "quizView"] [
    quizInfo m,
    currentQuestion m
  ]


quizInfo :: Model -> View Action
quizInfo Model{quiz, taker} = header_ [class_ "card"] [
    span_ [] [ezText $ "User: " ++ taker],
    span_ [] [ezText $ "File: " ++ "?"],
    span_ [] [text "Elapsed Time: ?"],
    span_ [] [ezText $ "Question " ++ show totalAnswered ++ "/" ++ show totalQuestions],
    progress_ [max_ . ms . show $ totalQuestions, value_ . ms . show $ totalAnswered] []
  ]
  where totalQuestions = Q.total quiz
        totalAnswered = Q.totalAnswered quiz


currentQuestion :: Model -> View Action
currentQuestion Model {quiz} = form_ [name_ "Quiz Question", onSubmit Finish, class_ "card"]
  [ header_
      []
      [ h2_ [] [text (ms quest)]
      ],
    section_ [] [answers (Q.currAnswer quiz)],
    footer_
      []
      [ nextButton,
        finishButton
      ]
  ]
  where 
    (Q.Question quest _ _, _) = Q.currAnswer quiz
    nextButton = button_ [onClick Next, type_ "button", class_ "button_light"] [text "Next"]
    finishButton = button_ [type_ "submit", class_ "button_dark"] [text "Finish"]

answers :: Q.AnsweredQuestion -> View Action
answers (Q.Question _ answers _, ans) = fieldset_ [class_ "checkbox_group"] (legend : items)
  where
    item :: (Int, String) -> View Action -- Vomiting over having to put inputs inside labels :/
    item (i, x) =
      div_ [class_ "checkbox"] [
          label_[for_ . ms $ x][text (ms x)], 
          input_ [type_ "checkbox", value_ (ms i), checked_ (isChecked ans i), 
            onClick (Answer i), id_ . ms $ x]
        ]
        
    isChecked :: Maybe Int -> Int -> Bool
    isChecked Nothing = const False
    isChecked (Just a) = (== a)
    legend = legend_ [] [text "Select an Answer"]
    items = item <$> zip [1 ..] answers

ezText :: (ToMisoString s) => s -> View Action
ezText = text . ms


viewPastResults :: Model -> View Action
viewPastResults m@Model{pastResults} = div_ [] [
    header_ [] [h2_ [] [text "Past Results"]],
    section_ [class_ "pastResults"] results,
    footer_ [] [ text "Back to home goes here"]
  ]
  where results = result <$> pastResults