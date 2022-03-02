module Unit (runUnitTest) where

import Data.Either (isRight)
import QuizParser (cleanQuizString, parseQuiz)
import Test.HUnit (Assertable (assert), Test (TestCase, TestLabel, TestList), runTestTT)
import qualified Test.HUnit as Test.HUnit.Base

test_shouldBeAbleToParseDefault :: Test
test_shouldBeAbleToParseDefault = TestCase $
  do
    quiz <- readFile "data/default"
    let res = (parseQuiz . cleanQuizString) quiz
    assert $ isRight res

test_shouldBeAbleToParseSample :: Test
test_shouldBeAbleToParseSample = TestCase $ do
  quiz <- readFile "data/sample"
  let res = parseQuiz . cleanQuizString $ quiz
  assert $ isRight res

test_shouldBeAbleToParse100 :: Test
test_shouldBeAbleToParse100 = TestCase $ do
  quiz <- readFile "data/test100"
  let res = parseQuiz . cleanQuizString $ quiz
  assert $ isRight res

runUnitTest :: IO Test.HUnit.Base.Counts
runUnitTest =
  runTestTT $
    TestList
      [ TestLabel "Default.q" test_shouldBeAbleToParseDefault,
        TestLabel
          "Sample.q"
          test_shouldBeAbleToParseSample
      ]