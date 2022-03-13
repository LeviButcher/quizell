{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import CLI (numberAnswers)
import Control.Monad ()
import Data.Char (isDigit)
import Data.Either (isLeft)
import Data.List (isInfixOf, isSubsequenceOf)
import QuestionParser (parseQuestions)
import Quiz (Question (Question), QuestionList, Quiz, shuffleQuestions, toPOSIXFileString, toWindowsFileString)
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

instance Arbitrary Question where
  arbitrary = do
    let randomLetter = choose ('a', 'Z')
    question <- listOf1 randomLetter
    answers <- listOf1 (listOf1 randomLetter)
    Question question answers <$> choose (0, length answers)

invalidLines :: String -> Bool
invalidLines ('*' : _) = True
invalidLines ('\r' : _) = True
invalidLines [] = True
invalidLines _ = False

prop_posixFileStringParseCorrectly :: QuestionList -> Bool
prop_posixFileStringParseCorrectly s =
  let res = (parseQuestions . toPOSIXFileString) s
   in case res of
        Left err -> False
        Right quiz -> quiz == s

prop_windowsFileStringParseCorrectly :: QuestionList -> Bool
prop_windowsFileStringParseCorrectly s =
  let res = (parseQuestions . toWindowsFileString) s
   in case res of
        Left err -> False
        Right quiz -> quiz == s

prop_shuffleQuestionsShouldShuffle :: Test.QuickCheck.Property.Property
prop_shuffleQuestionsShouldShuffle = monadicIO $ do
  quiz <- run $ generate $ listOf arbitrary
  rng <- run newStdGen
  let shuffled = shuffleQuestions rng quiz
  assert (length quiz == length shuffled)
  if length quiz <= 1 then assert (quiz == shuffled) else assert (quiz /= shuffled)

prop_invalidStringFails :: Gen Test.QuickCheck.Property.Result
prop_invalidStringFails = do
  s <- listOf1 arbitrary
  return $ case parseQuestions s of
    Left l -> if isInfixOf "Error Parsing Quiz File" $ show l then succeeded else failed
    Right r -> failed

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