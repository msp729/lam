{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Spoon (spoon)
import qualified Data.Text.IO as T
import Lang (prepare, simp)
import Logging (forget, writeLog)
import Parsers (lang)
import Text.Megaparsec (errorBundlePretty, parse)
import Prelude

main :: IO ()
main = do
    putStrLn "Please provide a lambda-calculus expression:"
    line <- T.getLine
    putStrLn "And would you like debug output? (1 for yes, anything else for no)"
    putStrLn "Be warned, it is long and (mostly) useless"
    dbg <- getLine
    case parse lang "<stdin>" line of
        Left e -> putStrLn ">:(" >> putStr (errorBundlePretty e)
        Right ex ->
            if spoon (read dbg :: Integer) == Just 1
                then print =<< (writeLog $ simp =<< prepare <$> simp ex)
                else print $ forget $ simp =<< prepare <$> simp ex
