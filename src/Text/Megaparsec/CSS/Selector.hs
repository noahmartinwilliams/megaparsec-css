module Text.Megaparsec.CSS.Selector where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.CSS.Combin
import Text.Megaparsec.CSS.Ident
import Text.Megaparsec.CSS.Types

cssTypeSelector :: CSSParser TypeSelector
cssTypeSelector = do
    id <- cssIdent
    return (TSName id)

cssClassSelector :: CSSParser ClassSelector
cssClassSelector = do
    void $ single '.'
    id <- cssIdent
    return (ClassSelSingle id)

cssPseudoElement :: CSSParser PseudoElement 
cssPseudoElement = do
    void $ string "::"
    id <- cssIdent
    return (PEString id)

cssCompoundSelector1 :: CSSParser CompoundSelector
cssCompoundSelector1 = do
    ts <- cssTypeSelector
    return (CSTypeSelector ts)

cssCompoundSelector2 :: CSSParser CompoundSelector
cssCompoundSelector2 = do
    ts <- cssTypeSelector
    cs <- cssClassSelector
    pe <- cssPseudoElement
    return (CSClass ts cs pe)

cssCompoundSelector :: CSSParser CompoundSelector
cssCompoundSelector = (try cssCompoundSelector2 <|> try cssCompoundSelector1)

cssSelector1 :: CSSParser Selector
cssSelector1 = do
    comp1 <- cssCompoundSelector 
    void $ hspace1
    comb <- cssCombin
    comp2 <- cssCompoundSelector
    return (SelectorCombin comp1 comb comp2)

cssSelector :: CSSParser Selector
cssSelector = cssSelector1
