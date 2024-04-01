{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding (getLine)

import Data.Text.IO (getLine)

import Lang (simp)
import Parsers (lang)
import Text.Megaparsec (parse)

main :: IO ()
main = do
    line <- getLine
    either (const $ return ()) (print . simp) $ parse lang "<stdin>" line
