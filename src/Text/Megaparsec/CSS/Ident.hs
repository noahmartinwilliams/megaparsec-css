module Text.Megaparsec.CSS.Ident where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.CSS.Combin
import Text.Megaparsec.CSS.Types

cssIdentChar :: CSSParser Char
cssIdentChar = (alphaNumChar <|> single '-')

cssIdent  :: CSSParser String
cssIdent = do
    name <- some cssIdentChar
    return name
