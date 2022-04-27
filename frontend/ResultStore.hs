{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module ResultStore where

import Miso.FFI.Storage
import qualified Quiz as Q
import qualified QuizCLI as CLI
import qualified QuizResults as R
import Model
import GHCJS.Foreign.Callback
import GHCJS.Types
import Miso.String
import Data.JSString
import Miso hiding (asyncCallback)
import Language.Javascript.JSaddle.Value (valToText, valToStr, deRefVal, JSValue(..))
import qualified Data.Text as Text


-- localstorage key/value
-- results/show [Q.QuizResults]


-- Add on result to already stored result string
storeResults :: Model -> IO ()
storeResults Model{quiz, startTime, taker} = do
    store <- localStorage
    storedRes <- readResults
    consoleLog (ms . show $ storedRes)
    setItem store "results" (ms . show $ (result:storedRes))
    where 
        result = R.getResults taker "?" quiz startTime startTime allotedTime
        allotedTime = 0
        

readResults :: IO [R.QuizResults]
readResults = do
    store <- localStorage
    rawJS <- getItem store "results"
    jsValue <- deRefVal rawJS
    case jsValue of
        (ValString x) -> return . read . Text.unpack $ x
        _ -> return []
    