module Text.Megaparsec.CSS.Combin where

import Text.Megaparsec
import Text.Megaparsec.CSS.Types

cssCombin :: CSSParser Combin
cssCombin = do
    c <- ((single '>') <|> (single '.'))
    case c of
        '>' -> return Child
        '.' -> return Period
