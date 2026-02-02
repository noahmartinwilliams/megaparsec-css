module Text.Megaparsec.CSS.Declarations where

import Control.Monad
import Data.Maybe
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
    return (ColorDeclaration ct cv False)

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
    return (SizeDeclaration st sv False)

cssVisibilityDeclaration :: CSSParser Declaration
cssVisibilityDeclaration = do
    void $ string "visibility"
    void $ hspace
    void $ single ':'
    void $ hspace
    void $ string "hidden"
    void $ hspace
    void $ single ';'
    return (VisibilityDeclaration Hidden False)


cssImportant :: CSSParser ()
cssImportant = do
    void $ hspace
    void $ single '!'
    void $ hspace
    void $ string "important" 
    return ()

getDisplayType :: String -> DisplayType 
getDisplayType "block" = DBlock
getDisplayType "inline-block" = DInlineBlock
getDisplayType "none" = DNone
getDisplayType "flex" = DFlex
getDisplayType "grid" = DGrid

cssDisplayDeclaration :: CSSParser Declaration
cssDisplayDeclaration = do
    void $ hspace
    void $ string "display"
    void $ hspace
    void $ single ':'
    void $ hspace
    value <- (string "block" <|> string "inline-block" <|> string "none" <|> string "flex" <|> string "grid")
    void $ hspace
    isImportant <- optional cssImportant
    void $ hspace
    void $ single ';'
    if isNothing isImportant
    then
        return (DisplayDeclaration (getDisplayType value) False)
    else
        return (DisplayDeclaration (getDisplayType value) True)

cssDeclaration :: CSSParser Declaration
cssDeclaration = (cssSizeDeclaration <|> cssColorDeclaration <|> cssVisibilityDeclaration)

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

