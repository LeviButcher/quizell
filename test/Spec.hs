{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad ()
import Data.Char (isDigit)
import Data.Either (isLeft)
import Data.List (isInfixOf, isSubsequenceOf)
import Lib (Quiz, QuizQuestion (QuizQuestion), QuizQuestions, numberAnswers, shuffleQuiz, toPOSIXFileString, toWindowsFileString)
import QuizParser (cleanQuizString, parseQuiz, quiz)
import System.Random (newStdGen)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Result,
    choose,
    listOf,
    listOf1,
    quickCheckAll,
  )
import Test.QuickCheck.Gen (generate)
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.QuickCheck.Property (classify, failed, succeeded)
import qualified Test.QuickCheck.Property
import Text.ParserCombinators.Parsec
  ( ParseError,
  )
import Unit (runUnitTest)
import Utils (allTrue, boundWrapAround, joinDelim)

instance Arbitrary QuizQuestion where
  arbitrary = do
    let randomLetter = choose ('a', 'Z')
    question <- listOf1 randomLetter
    answers <- listOf1 (listOf1 randomLetter)
    QuizQuestion question answers <$> choose (0, length answers)

invalidLines :: String -> Bool
invalidLines ('*' : _) = True
invalidLines ('\r' : _) = True
invalidLines [] = True
invalidLines _ = False

prop_posixFileStringParseCorrectly :: QuizQuestions -> Bool
prop_posixFileStringParseCorrectly s =
  let res = (parseQuiz . cleanQuizString . toPOSIXFileString) s
   in case res of
        Left err -> False
        Right quiz -> quiz == s

prop_windowsFileStringParseCorrectly :: QuizQuestions -> Bool
prop_windowsFileStringParseCorrectly s =
  let res = (parseQuiz . cleanQuizString . toWindowsFileString) s
   in case res of
        Left err -> False
        Right quiz -> quiz == s

prop_shuffleQuizShouldShuffle :: Test.QuickCheck.Property.Property
prop_shuffleQuizShouldShuffle = monadicIO $ do
  quiz <- run $ generate $ listOf arbitrary
  rng <- run newStdGen
  let shuffled = shuffleQuiz rng quiz
  assert (length quiz == length shuffled)
  if length quiz <= 1 then assert (quiz == shuffled) else assert (quiz /= shuffled)

prop_invalidStringFails :: Gen Test.QuickCheck.Property.Result
prop_invalidStringFails = do
  s <- listOf1 arbitrary
  return $ case parseQuiz s of
    Left l -> if isInfixOf "Error Parsing Quiz File" $ show l then succeeded else failed
    Right r -> failed

prop_QuizQuestionShows :: QuizQuestion -> Bool
prop_QuizQuestionShows q =
  let QuizQuestion qu a c = q
   in show q == show (QuizQuestion qu a c)

prop_numberLines :: [String] -> Bool
prop_numberLines s =
  let n = numberAnswers s
   in concatMap show [1 .. length s] `isSubsequenceOf` n

newtype FakeGetLine a = FakeGetLine a

instance Functor FakeGetLine where
  fmap f x = pure f <*> x

instance Applicative FakeGetLine where
  pure a = FakeGetLine a
  (<*>) (FakeGetLine f) (FakeGetLine a) = pure $ f a

instance Monad FakeGetLine where
  (>>=) (FakeGetLine a) f = f a

return []

main :: IO Bool
main = runUnitTest >> $quickCheckAll