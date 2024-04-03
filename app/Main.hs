{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text.IO (getLine)
import Lang (prepare, simp)
import Parsers (lang)
import Text.Megaparsec (errorBundlePretty, parse)
import Prelude hiding (getLine)
import Logging (writeLog)

main :: IO ()
main = do
    line <- getLine
    case parse lang "<stdin>" line of
        Left e -> putStrLn ">:(" >> putStr (errorBundlePretty e)
        Right ex -> do
            print ex
            ex' <- writeLog $ simp ex
            ex'' <- writeLog $ simp $ prepare ex'
            print ex''
