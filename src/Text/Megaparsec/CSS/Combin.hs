module Text.Megaparsec.CSS.Combin where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.CSS.Types

cssCombin1 :: CSSParser Combin
cssCombin1 = do
    c <- ((single '>') <|> (single '.'))
    case c of
        '>' -> do
            void $ hspace1 
            return Child
        '.' -> return Period

cssCombin2 :: CSSParser Combin
cssCombin2 = do
    void $ lookAhead (single '{')
    return NoCombin

cssCombin3 :: CSSParser Combin
cssCombin3 = do
    void $ lookAhead (single ',')
    return NoCombin

cssCombin = (try cssCombin1 <|> try cssCombin2 <|> try cssCombin3 )
