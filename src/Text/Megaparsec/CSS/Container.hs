module Text.Megaparsec.CSS.Container where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.CSS.Declarations
import Text.Megaparsec.CSS.Types

cssMediaType :: CSSParser MediaType
cssMediaType = do
    t <- (string "all" <|> string "print" <|> string "screen")
    case t of
        "all" -> return MTAll
        "print" -> return MTPrint
        "screen" -> return MTScreen

cssMediaContainer :: CSSParser Container
cssMediaContainer = do
    void $ string "@media"
    void $ hspace1
    mt <- cssMediaType
    void $ hspace1
    void $ single '{'
    void $ hspace
    rs <- some cssRuleSet
    void $ hspace
    void $ single '}'
    void $ hspace
    return (MediaContainer (CIMedia mt)  rs)

cssContainer :: CSSParser Container
cssContainer = cssMediaContainer 
