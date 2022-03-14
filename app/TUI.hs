{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DatatypeContexts #-}

module TUI
  ( quizApp,
    startState,
    QuizState (..),
  )
where

import Brick
    ( attrMap,
      attrName,
      continue,
      halt,
      neverShowCursor,
      suspendAndResume,
      fg,
      (<+>),
      (<=>),
      emptyWidget,
      hBox,
      hLimit,
      padLeftRight,
      str,
      strWrap,
      vBox,
      withAttr,
      AttrMap,
      App(..),
      EventM,
      Widget,
      BrickEvent(VtyEvent),
      Next )
import Brick.Widgets.Border ( border, borderWithLabel )
import Brick.Widgets.Center ( center, hCenter, vCenter )
import Control.Lens ( (&), (%~), (.~), element )
import qualified Data.Bifunctor
import Data.Either (fromRight)
import Data.List (group, intersperse)
import Data.List.Zipper (cursor)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime, nominalDay)
import Graphics.Vty ( defAttr, blue, green, red )
import qualified Graphics.Vty as V
import qualified Quiz as Q
import QuizResults ( QuizResults(QuizResults), getResults )
import Text.Printf (PrintfType, printf)
import Utils (boundWrapAround, getTimeString)

-- I'd like to change this to avoid misfeature DatatypeContexts
data (Q.Quiz q) => QuizState q = QuizState
  { quiz :: q,
    startTime :: UTCTime,
    finishedQuiz :: Bool,
    endTime :: Maybe UTCTime
  }

data Navigation = LEFT | RIGHT deriving (Eq, Show)

selectAnswer :: (Q.Quiz q) => Q.Direction -> QuizState q -> QuizState q
selectAnswer n q =
  q
    { quiz = Q.directionAnswerCurr (quiz q) n
    }

startState :: (Q.Quiz q) => Q.QuestionList -> IO (Maybe (QuizState q))
startState q = do
  quizStart <- getCurrentTime
  let mQuiz = Q.createQuiz q
  return $ QuizState <$> mQuiz <*> pure quizStart <*> pure False <*> Just Nothing

topUI :: (Q.Quiz q) => QuizState q -> Widget ()
topUI (QuizState q _ _ _) =
  let questionCount = Q.total q
      current = Q.currPosition q
   in border . hCenter . str $ "Question " ++ show current ++ "/" ++ show questionCount

bottomUI :: Widget ()
bottomUI =
  border . hCenter
    . hBox
    . intersperse (str "    ")
    $ str <$> ["Ctrl+C: Quit Quiz", "Right/Left: Next/Prev", "Up/Down: Select Answer", "Enter: Finish Quiz"]

resultUI :: (Q.Quiz q) => QuizState q -> Widget ()
resultUI (QuizState a s True e) =
  let (QuizResults answered total correct _ _) = getResults "" "" a
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

questionUI :: (Q.Quiz q) => QuizState q -> Widget ()
questionUI (QuizState quiz s done _) =
  let (Q.Question q a ci, s) = Q.currAnswer quiz
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
moveQuestion :: (Q.Quiz q) => Navigation -> QuizState q -> QuizState q
moveQuestion RIGHT q = q { quiz = (\l -> if Q.hasNext l then Q.next l else l) . quiz $ q}
moveQuestion LEFT q = q {quiz = Q.prev . quiz $ q}

drawUI :: (Q.Quiz q) => QuizState q -> [Widget ()]
drawUI qs =
  let mainUI = topUI qs <=> questionUI qs <=> bottomUI
   in if finishedQuiz qs then [resultUI qs <+> mainUI] else [mainUI]

endQuiz :: (Q.Quiz q) => QuizState q -> IO (QuizState q)
endQuiz st = do
  quizEnd <- getCurrentTime
  return $  st {
    finishedQuiz = True,
    endTime = Just quizEnd
  }

handleEvents :: (Q.Quiz q) => QuizState q -> BrickEvent () e -> EventM () (Next (QuizState q))
handleEvents st (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt st
handleEvents st (VtyEvent (V.EvKey V.KDown [])) = continue $ if finishedQuiz st then st else selectAnswer Q.Up st
handleEvents st (VtyEvent (V.EvKey V.KUp [])) = continue $ if finishedQuiz st then st else selectAnswer Q.Down st
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

quizApp :: (Q.Quiz q) => App (QuizState q) () ()
quizApp =
  App
    { appDraw = drawUI,
      appHandleEvent = handleEvents,
      appAttrMap = quizAttrMap,
      appStartEvent = return,
      appChooseCursor = neverShowCursor
    }