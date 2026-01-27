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

cssClassSelector1 :: CSSParser ClassSelector
cssClassSelector1 = do
    void $ single '.'
    id <- cssIdent
    return (ClassSelSingle id)

cssClassSelector2 :: CSSParser ClassSelector
cssClassSelector2 = do
    pc <- cssPseudoClass
    return (ClassSelPseudo pc)

cssClassSelector3 :: CSSParser ClassSelector
cssClassSelector3 = do
    ident <- cssIdent
    pc <- cssPseudoClass
    return (ClassSelIdentPseudo ident pc)

cssClassSelector :: CSSParser ClassSelector
cssClassSelector = (try cssClassSelector3 <|> try cssClassSelector2 <|> try cssClassSelector1)

cssPseudoElement :: CSSParser PseudoElement 
cssPseudoElement = do
    void $ string "::"
    id <- cssIdent
    return (PEString id)

cssPseudoClass1 :: CSSParser PseudoClass
cssPseudoClass1 = do
    void $ single ':'
    id <- cssIdent
    return (PCString id)

cssPseudoClass2 :: CSSParser PseudoClass
cssPseudoClass2 = do
    void $ single ':'
    id <- cssIdent
    void $ single '('
    void $ single '['
    sels <- cssCompoundSelector1
    void $ single ']'
    void $ single ')'
    void $ space
    return (PCFunctional id [(SelectorCombin [(sels, NoCombin)])])

cssPseudoClass :: CSSParser PseudoClass
cssPseudoClass = (try cssPseudoClass2 <|> try cssPseudoClass1)
    
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

cssSelectorPart :: CSSParser (CompoundSelector, Combin)
cssSelectorPart = do
    comp1 <- cssCompoundSelector 
    comb <- cssCombin
    return (comp1, comb)

cssSelector :: CSSParser Selector
cssSelector = do
    sels <- some cssSelectorPart
    return (SelectorCombin sels)

cssSep :: CSSParser ()
cssSep = do
    void $ space
    void $ single ','
    void $ space
    return ()

cssSelectors :: CSSParser [Selector]
cssSelectors = do
    sels <- cssSelector `sepBy` cssSep
    return sels
