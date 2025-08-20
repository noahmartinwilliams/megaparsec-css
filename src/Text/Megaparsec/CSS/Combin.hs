{-# LANGUAGE OverloadedStrings #-}
module Text.Megaparsec.CSS.Combin where

import Control.Monad
import Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.CSS.Space as S
import Text.Megaparsec.CSS.Selector
import Text.Megaparsec.CSS.Types

cssCombinator :: Parser Combinator
cssCombinator = do
    str <- ((string ">") <|> (string "||"))
    case str of 
        ">" -> return Child
        "||" -> return Column 


cssCombin1 :: Parser Combin
cssCombin1 = do
    s1 <- S.lexeme cssSelector
    c <- S.lexeme cssCombinator
    s2 <- S.lexeme cssSelector
    return (Combin s1 c s2)

cssCombin2 :: Parser Combin
cssCombin2 = do
    s <- S.lexeme cssSelector
    return (CombinSel s)

cssCombin :: Parser Combin
cssCombin = try cssCombin1 <|> cssCombin2
