module Text.Megaparsec.CSS.Declarations where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.CSS.Colors
import Text.Megaparsec.CSS.Selector
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
    void $ hspace
    return (ColorDeclaration ct cv)

cssSizeDeclaration :: CSSParser Declaration 
cssSizeDeclaration = do
    st <- cssSizeType 
    void $ hspace
    void $ single ':'
    void $ hspace
    sv <- cssSizeVal
    void $ hspace
    void $ single ';'
    void $ hspace
    return (SizeDeclaration st sv)

cssDeclaration :: CSSParser Declaration
cssDeclaration = (cssSizeDeclaration <|> cssColorDeclaration)

cssRuleSet :: CSSParser RuleSet
cssRuleSet = do
    sels <- cssSelectors
    void $ hspace
    void $ single '{'
    void $ hspace
    block <- some cssDeclaration
    void $ hspace
    void $ single '}'
    return (RuleSet sels block)
