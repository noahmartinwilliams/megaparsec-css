module Text.Megaparsec.CSS.Selector where

import Control.Monad
import Data.Maybe
import Data.Set as Set
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.CSS.Combin
import Text.Megaparsec.CSS.Ident
import Text.Megaparsec.CSS.Types

cssIdSelector :: CSSParser IDSelector
cssIdSelector = do
    void $ single '#'
    id <- cssIdent
    return (IDSel id)

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
    ts <- optional cssTypeSelector
    is <- many cssIdSelector
    cs <- many cssClassSelector
    pe <- optional cssPseudoElement
    if (isNothing pe) && (isNothing ts) && (is == []) && (cs == [])
    then
        fancyFailure (Set.fromList [ErrorFail "No Selector given."])
    else
        return (CSClass ts is cs pe)

cssCompoundSelector :: CSSParser CompoundSelector
cssCompoundSelector = (try cssCompoundSelector2 <|> try cssCompoundSelector1)

cssSelector :: CSSParser (CompoundSelector, Combin)
cssSelector = do
    comp1 <- cssCompoundSelector 
    void $ hspace
    comb <- cssCombin
    void $ hspace
    return (comp1, comb)

cssSelectors :: CSSParser Selector
cssSelectors = do
    sels <- some cssSelector
    void $ lookAhead (single '{')
    return (SelectorCombin sels)

