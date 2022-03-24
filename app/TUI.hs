module TUI where

-- import Brick
--   ( App (..),
--     AttrMap,
--     BrickEvent (VtyEvent),
--     EventM,
--     Next,
--     Widget,
--     attrMap,
--     attrName,
--     continue,
--     emptyWidget,
--     fg,
--     hBox,
--     hLimit,
--     halt,
--     neverShowCursor,
--     padLeftRight,
--     str,
--     strWrap,
--     suspendAndResume,
--     vBox,
--     withAttr,
--     (<+>),
--     (<=>),
--   )
-- import Brick.Widgets.Border (border, borderWithLabel)
-- import Brick.Widgets.Center (center, hCenter, vCenter)
-- import Control.Concurrent.MState (evalMState, execMState)
-- import Control.Lens (element, (%~), (&), (.~))
-- import Control.Monad.State.Lazy (StateT (runStateT), evalStateT, execStateT)
-- import qualified Data.Bifunctor
-- import Data.Either (fromRight)
-- import Data.Functor.Identity
-- import Data.List (group, intersperse)
-- import Data.Maybe (catMaybes, fromMaybe)
-- import Data.Time (UTCTime, defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime, nominalDay)
-- import Graphics.Vty (blue, defAttr, green, red)
-- import qualified Graphics.Vty as V
-- import qualified Quiz as Q
-- import QuizResults (QuizResults (QuizResults), getResults)
-- import Text.Printf (PrintfType, printf)
-- import Utils (boundWrapAround, getTimeString)

-- -- I'd like to change this to avoid misfeature DatatypeContexts
-- data QuizState = QuizState
--   { quiz :: Q.Quiz,
--     startTime :: UTCTime,
--     finishedQuiz :: Bool,
--     endTime :: Maybe UTCTime
--   }

-- data Navigation = LEFT | RIGHT deriving (Eq, Show)

-- execQuizState stateFunc state = runIdentity $ execMState stateFunc state

-- evalQuizState stateFunc state = runIdentity $ evalMState stateFunc state

-- selectAnswer :: Q.Direction -> QuizState -> QuizState
-- selectAnswer n q =
--   q
--     { quiz = execQuizState (Q.directionalAnswerCurr n) (quiz q)
--     }

-- startState :: Q.QuestionList -> IO QuizState
-- startState q = do
--   quizStart <- getCurrentTime
--   let mQuiz = Q.createQuiz q
--   return $ QuizState mQuiz quizStart False Nothing

-- topUI :: QuizState -> Widget ()
-- topUI (QuizState q _ _ _) =
--   let questionCount = Q.total q
--       current = Q.currPosition q
--    in border . hCenter . str $ "Question " ++ show current ++ "/" ++ show questionCount

-- bottomUI :: Widget ()
-- bottomUI =
--   border . hCenter
--     . hBox
--     . intersperse (str "    ")
--     $ str <$> ["Ctrl+C: Quit Quiz", "Right/Left: Next/Prev", "Up/Down: Select Answer", "Enter: Finish Quiz"]

-- resultUI :: QuizState -> Widget ()
-- resultUI (QuizState a s True e) =
--   let (QuizResults answered total correct _ _) = getResults "" "" a
--       elapsedTime = fromMaybe nominalDay $ Just diffUTCTime <*> e <*> Just s
--    in borderWithLabel (str "Result") . padLeftRight 2
--         . vCenter
--         $ str ("Answered: " ++ show answered ++ "/" ++ show total)
--           <=> str ("Correct: " ++ show correct ++ "/" ++ show total)
--           <=> str (printf "Percentage: %.2f" ((fromIntegral correct / fromIntegral total * 100) :: Float))
--           <=> str (printf "Elapsed Time: " ++ getTimeString elapsedTime)
-- resultUI _ = emptyWidget

-- wrapOnlyIf :: Int -> Int -> String -> Widget n
-- wrapOnlyIf m n t
--   | n < m = str t
--   | otherwise = hLimit m . strWrap $ t

-- questionUI :: QuizState -> Widget ()
-- questionUI (QuizState quiz s done _) =
--   let (Q.Question q a ci, s) = evalQuizState Q.currAnswer quiz
--       questionWidget = borderWithLabel (str "Question") . hCenter . wrapOnlyIf 100 (length q) $ q
--       answerList = ("[ ] " ++) <$> a & (element (pred $ fromMaybe (-1) s) %~ (element 1 .~ 'x'))
--       selectedAnswerList =
--         center
--           . vBox
--           $ str <$> answerList
--             & if not done
--               then element (pred $ fromMaybe (-1) s) %~ withAttr (attrName "selected")
--               else
--                 (element (pred $ fromMaybe (-1) s) %~ withAttr (attrName "wrong"))
--                   . (element (pred ci) %~ withAttr (attrName "correct"))
--    in questionWidget <=> selectedAnswerList

-- -- Need to handle end of zip here
-- moveQuestion :: Navigation -> QuizState -> QuizState
-- moveQuestion RIGHT q = q {quiz = (\l -> if evalQuizState Q.hasNext l then execQuizState Q.next l else l) . quiz $ q}
-- moveQuestion LEFT q = q {quiz = execQuizState Q.prev (quiz q)}

-- drawUI :: QuizState -> [Widget ()]
-- drawUI qs =
--   let mainUI = topUI qs <=> questionUI qs <=> bottomUI
--    in if finishedQuiz qs then [resultUI qs <+> mainUI] else [mainUI]

-- endQuiz :: QuizState -> IO QuizState
-- endQuiz st = do
--   quizEnd <- getCurrentTime
--   return $
--     st
--       { finishedQuiz = True,
--         endTime = Just quizEnd
--       }

-- handleEvents :: QuizState -> BrickEvent () e -> EventM () (Next QuizState)
-- handleEvents st (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = halt st
-- handleEvents st (VtyEvent (V.EvKey V.KDown [])) = continue $ if finishedQuiz st then st else selectAnswer Q.Up st
-- handleEvents st (VtyEvent (V.EvKey V.KUp [])) = continue $ if finishedQuiz st then st else selectAnswer Q.Down st
-- handleEvents st (VtyEvent (V.EvKey V.KRight [])) = continue $ moveQuestion RIGHT st
-- handleEvents st (VtyEvent (V.EvKey V.KLeft [])) = continue $ moveQuestion LEFT st
-- handleEvents st (VtyEvent (V.EvKey V.KEnter [])) = suspendAndResume $ endQuiz st
-- handleEvents st _ = continue st

-- quizAttrMap :: b -> AttrMap
-- quizAttrMap =
--   const $
--     attrMap
--       Graphics.Vty.defAttr
--       [ (attrName "selected", fg blue),
--         (attrName "wrong", fg red),
--         (attrName "correct", fg green)
--       ]

-- quizApp :: App QuizState () ()
-- quizApp =
--   App
--     { appDraw = drawUI,
--       appHandleEvent = handleEvents,
--       appAttrMap = quizAttrMap,
--       appStartEvent = return,
--       appChooseCursor = neverShowCursor
--     }