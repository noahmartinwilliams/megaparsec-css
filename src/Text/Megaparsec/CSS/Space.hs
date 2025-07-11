module Text.Megaparsec.CSS.Space where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

sc :: (Stream s, Token s ~ Char, Tokens s ~ String) => Parsec Void s ()
sc = L.space space1 empty empty

lexeme :: (Stream s, Token s ~ Char, Tokens s ~ String) => Parsec Void s a -> Parsec Void s a
lexeme = L.lexeme sc
