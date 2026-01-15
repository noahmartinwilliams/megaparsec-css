module Text.Megaparsec.CSS.Combin where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.CSS.Types

cssCombin :: CSSParser Combin
cssCombin = do
    c <- ((single '>') <|> (single '.'))
    case c of
        '>' -> do
            void $ hspace1 
            return Child
        '.' -> return Period
