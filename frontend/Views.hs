{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Views where

import Data.Time.Clock
import Miso hiding (asyncCallback)
import Miso.String (ToMisoString, ms)
import Model
import QuestionParser
import qualified Quiz as Q
import qualified QuizCLI as CLI
import qualified QuizResults as R
import Data.Maybe (fromMaybe, isJust)

ezText :: (ToMisoString s) => s -> View Action
ezText = text . ms

layout :: (Model -> View Action) -> Model -> View Action
layout f m = section_ [class_ "siteLayout"] [
    title, 
    main_ [class_ "guttered"] [va],
    footer
  ]
  where
    va = f m
    title = header_ [] [div_ [class_ "guttered"] [
          h1_ [] [text "Quizell! - the haskell quiz taker"]
        ]
      ]
    footer = footer_ [] [
        div_ [class_ "guttered"] [
          span_ [] [text "Made with Haskell + Miso by Levi Butcher"],
          a_ [href_ "https://github.com/LeviButcher/quizell"] [text "View on Github"]
        ]
      ]

-- Determines what Views to render based on Model State
viewModel :: Model -> View Action
viewModel m@Model {state} = case state of
  Home -> viewHome m
  RunningQuiz -> viewTakingQuiz m
  Finished -> viewFinishScreen m
  QuizConfig -> viewQuizConfigForm m
  PastResults -> viewPastResults m
  UserForm -> viewUserForm m

-- Title Screen, Changes Based on if userName is entered
viewHome :: Model -> View Action
viewHome Model{taker=Nothing} =
  div_
    []
    [ h2_ [] [text "Welcome - Create a User first silly"],
      menu_ [class_ "home_menu"] [button_ [onClick ShowUserForm] [text "Change User Config"]]
    ]
viewHome Model{taker=Just name} = 
    div_
    []
    [ h2_ [] [ezText $ "Welcome " ++ name ++" - Select an Option"],
      menu_ [class_ "home_menu"] [
            button_ [onClick QuizFormStart] [text "Take a Quiz!"],
            button_ [onClick GetPastResults] [text "View Past Results"],
            button_ [onClick ShowUserForm] [text "Change User Config"]
          ]
    ]
      

-- View Shown on completion of a quiz
viewFinishScreen :: Model -> View Action
viewFinishScreen m =
  section_
    []
    [ res,
      goHomeButton
    ]
  where
    goHomeButton = button_ [onClick ShowHome] [text "Go Back To Home"]
    res = case getModelResults m of 
        Nothing -> div_ [] [text "Something Failed"]
        Just x -> viewResult x
    
viewResult :: R.QuizResults -> View Action
viewResult R.QuizResults {total, correct, taker, testFile, startTime, endTime, allotedTime} =
  article_
    [class_ "card"]
    [ header_ [] [h4_ [] [text "Results"]],
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
    viewQuizInfo m,
    viewCurrentQuestion m
  ]


viewQuizInfo :: Model -> View Action
viewQuizInfo Model{quiz, taker, allotedTime} = header_ [class_ "card"] [
    span_ [] [ezText $ "User: " ++ (fromMaybe "No name entered" taker)],
    span_ [] [ezText $ "File: " ++ "?"],
    span_ [] [ezText $ "Alloted Time (In Seconds): " ++ show allotedTime],
    span_ [] [text "Elapsed Time: ?"],
    span_ [] [ezText $ "Question " ++ show currQuestion ++ "/" ++ show totalQuestions],
    progress_ [max_ . msShow $ totalQuestions, value_ . msShow $ totalAnswered, min_ "0"] []
  ]
  where totalQuestions = Q.total quiz
        totalAnswered = Q.totalAnswered quiz
        currQuestion = Q.currPosition quiz
        msShow = ms . show


viewCurrentQuestion :: Model -> View Action
viewCurrentQuestion Model {quiz} = form_ [name_ "Quiz Question", onSubmit Finish, class_ "card"]
  [ header_
      []
      [ h2_ [] [text (ms quest)]
      ],
    section_ [class_ "inputGroup"] [answerCheckboxList (Q.currAnswer quiz)],
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

answerCheckboxList :: Q.AnsweredQuestion -> View Action
answerCheckboxList (Q.Question _ answers _, ans) = fieldset_ [class_ "checkbox_group"] (legend : items)
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




viewPastResults :: Model -> View Action
viewPastResults Model{pastResults} = section_ [class_ "pastResultsSection"] [
    header_ [] [h2_ [] [text "Past Results"]],
    section_ [class_ "pastResults"] results,
    footer_ [] [ button_  [onClick ShowHome, class_ "button_dark"] [text "To Home"]]
  ]
  where 
    results = case viewResult <$> pastResults of
      [] -> [p_ [] [text "You got no past results? Take some quizzes!"]]
      x -> x

-- FORMS
viewUserForm :: Model -> View Action
viewUserForm Model{formError} = form_ [onSubmit SubmitUserForm, class_ "card"] [
    header_ [] [h3_ [] [text "User Config Form"]],
    section_ [] [
      div_ [class_ "errorText"] (case formError of 
          Just x -> [p_ [] [ezText x]]
          Nothing -> []),
      div_ [class_ "inputGroup"] [
        label_ [for_ "name"] [text "User Name"],
        input_ [type_ "text", id_ "name", required_ True]
      ]
    ],
    footer_ [] [
      button_ [type_ "Submit", class_ "button_dark"] [text "Submit"]
    ]
  ]

viewQuizConfigForm :: Model -> View Action
viewQuizConfigForm Model{formError} =
  form_
    [id_ "quizForm", onSubmit QuizFormSubmit, class_ "card"]
    [ header_ [] [h2_ [] [text "Setup Quiz"]],
      section_
        []
        [
          div_ [class_ "errorText"] (case formError of 
            Just x -> [p_ [] [ezText x]]
            Nothing -> []),
          div_ [class_ "inputGroup"] [
            label_ [for_ "number"] [text "How many questions? (0 means all)"],
            input_ [type_ "number", name_ "questions", id_ "questions", min_ "0", 
              value_ "0", required_ True],
            label_ [for_ "allotedTime"] [text "Alloted Time (In Seconds)"],
            input_ [type_ "number", name_ "allotedTime", id_ "allotedTime", min_ "0", 
              value_ "0", required_ True],
            label_ [for_ "questionsFile"] [text "Upload Question List"],
            input_ [type_ "File", name_ "questionsFile", id_ "questionsFile", required_ True]
          ]
        ],
      footer_
        []
        [button_ [type_ "submit", class_ "button_dark"] [text "Submit"]]
    ]