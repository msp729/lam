module Parsers (lex, var, lam, ap, lang) where

import Control.Applicative (many)
import Data.Text (Text)
import Data.Void
import Lang
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char (char, space1)
import Text.Megaparsec.Char.Lexer (space)
import Prelude hiding (lex)

type Parser = Parsec Void Text

sp :: Parser ()
sp = space space1 empty empty

lex :: String -> Parser a -> Parser a
lex name = label name

var :: Parser Name
var = lex "variable name" $ takeWhile1P Nothing (\c -> all (/= c) " ,()[]{}\\\t\r\n")

lam :: Parser Name -> Parser Expr -> Parser Expr
lam pn pe = lex "lambda-expression" $ do
    _ <- char '\\' <|> char 'λ'
    n <- pn
    _ <- char ',' <|> char '.' <|> char ' '
    e <- pe
    return $ Lam n e

ap :: Parser Expr -> Parser Expr
ap pe = lex "function-application" $ do
    f <- pe
    sep <- (False <$ try space1) <|> (True <$ char '(')
    x <- pe
    if sep then () <$ char ')' else return ()
    return $ Ap f x

paren :: Parser a -> Parser a
paren = between (char '(') (char ')')

lang :: Parser Expr
lang = do
    sp
    args <- many $ do
        _ <- char '\\' <|> char 'λ'
        sp
        n <- var
        space1 <|> () <$ (char ',' <|> char '.')
        sp
        return n
    vals <- many $ flip (<|>) (paren lang <* sp) $ try $ Var <$> var <* sp
    let lams = Lam <$> args
    let clam = foldl (.) id lams
    let app = foldl1 Ap vals
    return $ clam app
