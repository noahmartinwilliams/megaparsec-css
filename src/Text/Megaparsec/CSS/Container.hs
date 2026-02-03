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
    void $ space
    void $ string "@media"
    void $ space
    mt <- cssMediaType
    void $ space
    void $ single '{'
    void $ space
    rs <- some cssRuleSet
    void $ space
    void $ single '}'
    void $ space
    return (MediaContainer (CIMedia mt)  rs)

cssContainer :: CSSParser Container
cssContainer = cssMediaContainer 
