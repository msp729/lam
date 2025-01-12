{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Trans.Class
import Data.Maybe
import Data.Text qualified as T
import Lang (prepare, simp)
import Logging (Level, writeLog)
import Parsers (lang, level)
import System.Console.Haskeline
import Text.Megaparsec (errorBundlePretty, parse)
import Prelude

main :: IO ()
main = runInputT defaultSettings app

app :: InputT IO ()
app = do
    outputStrLn "Debug level? (2 for high, 1 for medium, anything else for none)"
    outputStrLn "None is recommended"
    dbg <- fmap level <$> getInputLine "? "
    case dbg of
        Nothing -> return ()
        Just x -> interp x

interp :: Level -> InputT IO ()
interp dbg = do
    outputStrLn "Please provide a λ-calculus expression:"
    line <- (T.pack . fromMaybe ":q") <$> getInputLine "> "
    handle line dbg

handle :: T.Text -> Level -> InputT IO ()
handle ":q" _ = return ()
handle "" dbg = interp dbg
handle l dbg = do
    case parse lang "<stdin>" l of
        Left e -> outputStrLn "invalid" >> outputStr (errorBundlePretty e)
        Right ex -> (outputStrLn . show) =<< ((lift . writeLog dbg) $ simp =<< prepare <$> simp ex)
    interp dbg
