module Text.Megaparsec.CSS.Declarations where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.CSS.Colors
import Text.Megaparsec.CSS.Size
import Text.Megaparsec.CSS.Types

cssColorDeclaration :: CSSParser Declaration
cssColorDeclaration = do
    ct <- cssColorType
    void $ hspace
    void $ single ':'
    void $ hspace
    cv <- cssColorValNamed 
    void $ hspace
    void $ single ';'
    return (ColorDeclaration ct cv)

cssSizeDeclaration :: CSSParser Declaration 
cssSizeDeclaration = do
    st <- cssSizeType 
    void $ hspace
    void $ single ':'
    void $ hspace
    sv <- cssSizeVal
    return (SizeDeclaration st sv)
