{-# LANGUAGE TemplateHaskell #-}

module TUI
  ( quizApp,
    startState,
    QuizState (..),
  )
where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Lens
import qualified Data.Bifunctor
import Data.Either (fromRight)
import Data.List (group, intersperse)
import Data.List.Zipper (cursor)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime, nominalDay)
import Graphics.Vty
import qualified Graphics.Vty as V
import Quiz (QuizError, QuizResults (QuizResults), directionalAnswerCurrentQuestion, startQuiz)
import qualified Quiz as Q
import Text.Printf (PrintfType, printf)
import Utils (boundWrapAround, getTimeString)

data QuizState = QuizState
  { _quiz :: Q.Quiz,
    _startTime :: UTCTime,
    _finishedQuiz :: Bool,
    _endTime :: Maybe UTCTime
  }
  deriving (Show)

makeLenses ''QuizState

data Navigation = LEFT | RIGHT deriving (Eq, Show)

selectAnswer :: Q.Direction -> QuizState -> QuizState
selectAnswer n q = q & quiz %~ (fromRight (q ^. quiz) . directionalAnswerCurrentQuestion n)

startState :: Q.QuestionList -> IO QuizState
startState q = do
  quizStart <- getCurrentTime
  return
    QuizState
      { _quiz = startQuiz q,
        _startTime = quizStart,
        _endTime = Nothing,
        _finishedQuiz = False
      }

topUI :: QuizState -> Widget ()
topUI (QuizState q _ _ _) =
  let questionCount = Q.totalQuestions q
      current = Q.currentQuestionNumber q
   in border . hCenter . str $ "Question " ++ show current ++ "/" ++ show questionCount

bottomUI :: Widget ()
bottomUI =
  border . hCenter
    . hBox
    . intersperse (str "    ")
    $ str <$> ["Ctrl+C: Quit Quiz", "Right/Left: Next/Prev", "Up/Down: Select Answer", "Enter: Finish Quiz"]

resultUI :: QuizState -> Widget ()
resultUI (QuizState a s True e) =
  let (QuizResults answered total correct) = Q.getResults a
      elapsedTime = fromMaybe nominalDay $ Just diffUTCTime <*> e <*> Just s
   in borderWithLabel (str "Result") . padLeftRight 2
        . vCenter
        $ str ("Answered: " ++ show answered ++ "/" ++ show total)
          <=> str ("Correct: " ++ show correct ++ "/" ++ show total)
          <=> str (printf "Percentage: %.2f" ((fromIntegral correct / fromIntegral total * 100) :: Float))
          <=> str (printf "Elapsed Time: " ++ getTimeString elapsedTime)
resultUI _ = emptyWidget

wrapOnlyIf :: Int -> Int -> String -> Widget n
wrapOnlyIf m n t
  | n < m = str t
  | otherwise = hLimit m . strWrap $ t

questionUI :: QuizState -> Widget ()
questionUI (QuizState quiz s done _) =
  let (Q.Question q a ci, s) = cursor quiz
      questionWidget = borderWithLabel (str "Question") . hCenter . wrapOnlyIf 100 (length q) $ q
      answerList = ("[ ] " ++) <$> a & (element (pred $ fromMaybe (-1) s) %~ (element 1 .~ 'x'))
      selectedAnswerList =
        center
          . vBox
          $ str <$> answerList
            & if not done
              then element (pred $ fromMaybe (-1) s) %~ withAttr (attrName "selected")
              else
                (element (pred $ fromMaybe (-1) s) %~ withAttr (attrName "wrong"))
                  . (element (pred ci) %~ withAttr (attrName "correct"))
   in questionWidget <=> selectedAnswerList

-- Need to handle end of zip here
moveQuestion :: Navigation -> QuizState -> QuizState
moveQuestion RIGHT q = q & quiz %~ (\l -> if Q.isLastQuestion l then l else Q.nextQuestion l)
moveQuestion LEFT q = q & quiz %~ Q.prevQuestion

drawUI :: QuizState -> [Widget ()]
drawUI qs =
  let mainUI = topUI qs <=> questionUI qs <=> bottomUI
   in if qs ^. finishedQuiz then [resultUI qs <+> mainUI] else [mainUI]

endQuiz :: QuizState -> IO QuizState
endQuiz st = do
  quizEnd <- getCurrentTime
  return $ st & (finishedQuiz .~ True) . (endTime ?~ quizEnd)

handleEvents :: QuizState -> BrickEvent () e -> EventM () (Next QuizState)
handleEvents st (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt st
handleEvents st (VtyEvent (V.EvKey V.KDown [])) = continue $ if st ^. finishedQuiz then st else selectAnswer Q.Up st
handleEvents st (VtyEvent (V.EvKey V.KUp [])) = continue $ if st ^. finishedQuiz then st else selectAnswer Q.Down st
handleEvents st (VtyEvent (V.EvKey V.KRight [])) = continue $ moveQuestion RIGHT st
handleEvents st (VtyEvent (V.EvKey V.KLeft [])) = continue $ moveQuestion LEFT st
handleEvents st (VtyEvent (V.EvKey V.KEnter [])) = suspendAndResume $ endQuiz st
handleEvents st _ = continue st

quizAttrMap :: b -> AttrMap
quizAttrMap =
  const $
    attrMap
      Graphics.Vty.defAttr
      [ (attrName "selected", fg blue),
        (attrName "wrong", fg red),
        (attrName "correct", fg green)
      ]

quizApp :: App QuizState () ()
quizApp =
  App
    { appDraw = drawUI,
      appHandleEvent = handleEvents,
      appAttrMap = quizAttrMap,
      appStartEvent = return,
      appChooseCursor = neverShowCursor
    }