module Text.Megaparsec.CSS.Declarations where

import Control.Monad
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.CSS.Colors
import Text.Megaparsec.CSS.Selector
import Text.Megaparsec.CSS.Size
import Text.Megaparsec.CSS.Types

cssColorDeclaration :: CSSParser Declaration
cssColorDeclaration = do
    ct <- cssColorType
    void $ C.space
    void $ single ':'
    void $ C.space
    cv <- cssColorValNamed 
    void $ C.space
    void $ single ';'
    void $ C.space
    return (ColorDeclaration ct cv False)

cssSizeDeclaration :: CSSParser Declaration 
cssSizeDeclaration = do
    st <- cssSizeType 
    void $ C.space
    void $ single ':'
    void $ C.space
    sv <- cssSizeVal
    void $ C.space
    void $ single ';'
    void $ C.space
    return (SizeDeclaration st sv False)

cssVisibilityDeclaration :: CSSParser Declaration
cssVisibilityDeclaration = do
    void $ string "visibility"
    void $ C.space
    void $ single ':'
    void $ C.space
    void $ string "hidden"
    void $ C.space
    void $ single ';'
    void $ C.space
    return (VisibilityDeclaration Hidden False)


cssImportant :: CSSParser ()
cssImportant = do
    void $ C.space
    void $ single '!'
    void $ string "important" 
    void $ C.space
    return ()

getDisplayType :: String -> DisplayType 
getDisplayType "block" = DBlock
getDisplayType "inline-block" = DInlineBlock
getDisplayType "none" = DNone
getDisplayType "flex" = DFlex
getDisplayType "grid" = DGrid

cssDisplayDeclaration :: CSSParser Declaration
cssDisplayDeclaration = do
    void $ C.space
    void $ string "display"
    void $ C.space
    void $ single ':'
    void $ C.space
    value <- (string "block" <|> string "inline-block" <|> string "none" <|> string "flex" <|> string "grid")
    void $ C.space
    isImportant <- optional cssImportant
    void $ C.space
    void $ single ';'
    void $ C.space
    if isNothing isImportant
    then
        return (DisplayDeclaration (getDisplayType value) False)
    else
        return (DisplayDeclaration (getDisplayType value) True)

cssDeclaration :: CSSParser Declaration
cssDeclaration = (cssSizeDeclaration <|> cssColorDeclaration <|> cssVisibilityDeclaration <|> cssDisplayDeclaration)

cssRuleSet :: CSSParser RuleSet
cssRuleSet = do
    sels <- cssSelectors
    void $ C.space
    void $ single '{'
    void $ C.space
    block <- some cssDeclaration
    void $ C.space
    void $ single '}'
    void $ C.space
    return (RuleSet sels block)

