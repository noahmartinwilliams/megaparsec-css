{-# LANGUAGE FlexibleContexts #-}
module Text.Megaparsec.CSS.Declarations where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Text.Megaparsec.CSS.Types
import Text.Megaparsec.CSS.Colors
import Text.Megaparsec.CSS.Combin
import Text.Megaparsec.CSS.Space
import Text.Megaparsec.CSS.Size
import Text.Megaparsec.CSS.Selector

cssColorDeclaration :: Parser Declaration
cssColorDeclaration = do
    colorProp <- lexeme (cssColorProperty)
    void $ lexeme (single ':')
    colorVal <- lexeme (cssColorValNamed )
    void $ lexeme (single ';')
    return (ColorDeclaration colorProp colorVal)

cssSizeDeclaration :: Parser Declaration
cssSizeDeclaration = do
    sizeType <- lexeme (cssSizeType)
    void $ lexeme (single ':')
    sizeVal <- lexeme (cssSizeVal)
    void $ lexeme (single ';')
    return (SizeDeclaration sizeType sizeVal)

cssDeclaration :: Parser Declaration 
cssDeclaration = cssColorDeclaration <|> cssSizeDeclaration

cssDeclarationBlock :: Parser Block
cssDeclarationBlock = do
    combin <- lexeme (cssCombin )
    void $ lexeme (single '{')
    block <- lexeme (many cssDeclaration)
    void $ lexeme (single '}')
    return (Block combin block)
