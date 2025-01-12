{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Trans.Class
import Data.Maybe
import Data.Text qualified as T
import Lang (prepare, simp)
import Logging (forget, writeLog)
import Parsers (lang)
import System.Console.Haskeline
import Text.Megaparsec (errorBundlePretty, parse)
import Prelude

main :: IO ()
main = runInputT defaultSettings app

app :: InputT IO ()
app = do
    outputStrLn "Would you like debug output? (1 for yes, anything else for no)"
    outputStrLn "Be warned, it is long and (mostly) useless"
    dbg <- getInputLine "? "
    case dbg of
        Nothing -> return ()
        Just x -> interp (x == "1")

interp :: Bool -> InputT IO ()
interp dbg = do
    outputStrLn "Please provide a λ-calculus expression:"
    line <- (T.pack . fromMaybe ":q") <$> getInputLine "> "
    handle line dbg

handle :: T.Text -> Bool -> InputT IO ()
handle ":q" _ = return ()
handle "" dbg = interp dbg
handle l dbg = do
    case parse lang "<stdin>" l of
        Left e -> outputStrLn "invalid" >> outputStr (errorBundlePretty e)
        Right ex -> (outputStrLn . show) =<< ((if dbg then lift . writeLog else pure . forget) $ simp =<< prepare <$> simp ex)
    interp dbg
