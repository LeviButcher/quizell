module QuizParser
  ( cleanQuizString,
    question,
    parseQuiz,
    quiz,
  )
where

import qualified Data.List.Split as S
import qualified Quiz as Q
import Text.Parsec (endOfLine, (<|>))
import qualified Text.Parsec as P
import Text.ParserCombinators.Parsec
  ( GenParser,
    ParseError,
    anyChar,
    char,
    digit,
    eof,
    many,
    manyTill,
    optional,
    parse,
    string,
    try,
  )

invalidLines :: String -> Bool
invalidLines ('*' : _) = True
invalidLines [] = True
invalidLines _ = False

cleanQuizString :: String -> String
cleanQuizString = unlines . filter (not . invalidLines) . lines . filter (/= '\r')

parseQuiz :: String -> Either ParseError Q.QuestionList
parseQuiz = parse quiz "Error Parsing Quiz File"

quiz :: GenParser Char st Q.QuestionList
quiz = do
  quizSheet <- many quizQuestion
  eof
  return quizSheet

quizQuestion :: GenParser Char st Q.Question
quizQuestion = do
  q <- question
  (ci, a) <- answers
  return (Q.Question q a ci)

questionMarker :: GenParser Char st String
questionMarker = do
  char '@'
  char 'Q' <|> char 'q'
  manyTill anyChar (try endOfLine)

answerMarker :: GenParser Char st String
answerMarker = do
  char '@'
  char 'A' <|> char 'a'
  manyTill anyChar (try endOfLine)

endMarker :: GenParser Char st String
endMarker = do
  char '@'
  char 'E' <|> char 'e'
  manyTill anyChar (try endOfLine)

question :: GenParser Char st String
question = do
  questionMarker
  manyTill anyChar (try (endOfLine >> answerMarker))

answers :: GenParser Char st (Int, [String])
answers = do
  correctIndex <- read <$> many digit
  endOfLine
  answers <- manyTill lineOfText (try endMarker)
  optional endOfLine
  return (correctIndex, answers)

lineOfText :: GenParser Char st String
lineOfText = manyTill anyChar (char '\n')
