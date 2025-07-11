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
import Text.Megaparsec.CSS.Space

cssColorDeclaration :: (Stream s, Token s ~ Char, Tokens s ~ String) => Parsec Void s Declaration
cssColorDeclaration = do
    colorProp <- lexeme (cssColorProperty)
    void $ lexeme (single ':')
    colorVal <- lexeme (cssColorValNamed )
    void $ lexeme (single ';')
    return (ColorDeclaration colorProp colorVal)
