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
    putStrLn "Would you like debug output? (1 for yes, anything else for no)"
    putStrLn "Be warned, it is long and (mostly) useless"
    dbg <- getLine
    interp (spoon (read dbg :: Integer) == Just 1)

interp :: Bool -> IO ()
interp b = do
    putStrLn "Please provide a λ-calculus expression:"
    line <- T.getLine
    if line == ":q" then return () else
        if line == "" then interp b else do
            case parse lang "<stdin>" line of
                Left e -> putStrLn "invalid" >> putStr (errorBundlePretty e)
                Right ex ->
                    if b
                        then print =<< (writeLog $ simp =<< prepare <$> simp ex)
                        else print $ forget $ simp =<< prepare <$> simp ex
            interp b
