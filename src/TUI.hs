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
import Data.List (group, intersperse)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime, nominalDay)
import Graphics.Vty
import qualified Graphics.Vty as V
import Lib (QuizQuestion (answers))
import qualified Lib as L
import Text.Printf (PrintfType, printf)
import Utils (boundWrapAround)

type MaybeAnsweredQuestion = [(Maybe Int, L.QuizQuestion)]

data QuizState = QuizState
  { _answeredQuiz :: MaybeAnsweredQuestion,
    _currentQuestion :: Int,
    _startTime :: UTCTime,
    _finishedQuiz :: Bool,
    _endTime :: Maybe UTCTime
  }
  deriving (Show)

makeLenses ''QuizState

data Navigation = UP | DOWN deriving (Eq, Show)

-- Increase or Decrease the value inside the Maybe then return it
-- Don't allow value to go below 0 or above the Int passed in
-- Should wrap around if the upper or lower bounds is hit
nextSelected :: Int -> Navigation -> Maybe Int -> Maybe Int
nextSelected _ _ Nothing = Just 0
nextSelected b UP (Just x) = Just $ boundWrapAround succ 0 b x
nextSelected b DOWN (Just x) = Just $ boundWrapAround pred 0 b x

selectAnswer :: Navigation -> QuizState -> QuizState
selectAnswer d q =
  let QuizState ql cQ _ _ _ = q
   in q & answeredQuiz %~ element cQ
        %~ ( \y ->
               Data.Bifunctor.first (nextSelected (pred . length . answers . snd $ y) d) y
           )

startState :: L.QuizQuestions -> IO QuizState
startState q = do
  quizStart <- getCurrentTime
  return
    QuizState
      { _answeredQuiz = zip (repeat Nothing) q,
        _currentQuestion = 0,
        _startTime = quizStart,
        _endTime = Nothing,
        _finishedQuiz = False
      }

topUI :: QuizState -> Widget ()
topUI (QuizState q c s _ _) =
  border . hCenter . str $ "Question " ++ (show . succ $ c) ++ "/" ++ (show . length $ q)

bottomUI :: Widget ()
bottomUI =
  border . hCenter
    . hBox
    . intersperse (str "    ")
    $ str <$> ["Ctrl+C: Quit Quiz", "Right/Left: Next/Prev", "Up/Down: Select Answer", "Enter: Finish Quiz"]

transformFinishedQuiz :: (Maybe Int, L.QuizQuestion) -> Maybe (Int, L.QuizQuestion)
transformFinishedQuiz (Nothing, _) = Nothing
transformFinishedQuiz (Just x, y) = Just (x + 1, y)

resultUI :: QuizState -> Widget ()
resultUI (QuizState a _ s True e) =
  let y = catMaybes $ transformFinishedQuiz <$> a
      answered = length y
      total = length a
      correct = L.numberCorrect y
      elapsedTime = fromMaybe nominalDay $ Just diffUTCTime <*> e <*> Just s
   in borderWithLabel (str "Result") . padLeftRight 2
        . vCenter
        $ str ("Answered: " ++ show answered ++ "/" ++ show total)
          <=> str ("Correct: " ++ show correct ++ "/" ++ show total)
          <=> str (printf "Percentage: %.2f" ((fromIntegral correct / fromIntegral total * 100) :: Float))
          <=> str (printf "Elapsed Time: " ++ formatTime defaultTimeLocale "%hh:%mm:%ss" elapsedTime)
resultUI _ = emptyWidget

wrapOnlyIf :: Int -> Int -> String -> Widget n
wrapOnlyIf m n t
  | n < m = str t
  | otherwise = hLimit m . strWrap $ t

questionUI :: QuizState -> Widget ()
questionUI (QuizState quiz c s done _) =
  let (s, L.QuizQuestion q a ci) = quiz !! c
      questionWidget = borderWithLabel (str "Question") . hCenter . wrapOnlyIf 100 (length q) $ q
      answerList = ("[ ] " ++) <$> a & (element (fromMaybe (-1) s) %~ (element 1 .~ 'x'))
      selectedAnswerList =
        center
          . vBox
          $ str <$> answerList
            & if not done
              then element (fromMaybe (-1) s) %~ withAttr (attrName "selected")
              else
                (element (fromMaybe (-1) s) %~ withAttr (attrName "wrong"))
                  . (element (pred ci) %~ withAttr (attrName "correct"))
   in questionWidget <=> selectedAnswerList

moveQuestion :: Navigation -> QuizState -> QuizState
moveQuestion d q =
  let qLength = q ^. (answeredQuiz . to length)
   in q & currentQuestion %~ boundWrapAround (if d == UP then succ else pred) 0 (qLength - 1)

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
handleEvents st (VtyEvent (V.EvKey V.KDown [])) = continue $ if st ^. finishedQuiz then st else selectAnswer UP st
handleEvents st (VtyEvent (V.EvKey V.KUp [])) = continue $ if st ^. finishedQuiz then st else selectAnswer DOWN st
handleEvents st (VtyEvent (V.EvKey V.KRight [])) = continue $ moveQuestion UP st
handleEvents st (VtyEvent (V.EvKey V.KLeft [])) = continue $ moveQuestion DOWN st
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