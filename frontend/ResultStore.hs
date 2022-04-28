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
import Miso.String hiding (filter)
import Miso hiding (asyncCallback)
import Language.Javascript.JSaddle.Value (valToText, valToStr, deRefVal, JSValue(..))
import qualified Data.Text as Text


-- localstorage key/value
-- results/show [Q.QuizResults]


-- Add on result to already stored result string
storeResults :: Model -> IO ()
storeResults m@Model{taker} = do
    store <- localStorage
    storedRes <- readResults
    consoleLog (ms . show $ storedRes)
    case getModelResults m of
        Nothing -> return ()
        Just r -> setItem store "results" (ms . show $ (r:storedRes))

readResults :: IO [R.QuizResults]
readResults = do
    store <- localStorage
    rawJS <- getItem store "results"
    jsValue <- deRefVal rawJS
    case jsValue of
        (ValString x) -> return . read . Text.unpack $ x
        _ -> return []

getPastResults :: Model -> IO[R.QuizResults]
getPastResults Model{taker} = do
    case taker of
        Nothing -> return []
        Just x -> do
            allResults <- readResults
            return $ filter (\y -> R.taker y == x) allResults