module Text.Megaparsec.CSS.Space where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.CSS.Types
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
