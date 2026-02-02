module Text.Megaparsec.CSS ( cssDoc, module Text.Megaparsec.CSS.Types) where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.CSS.Container
import Text.Megaparsec.CSS.Declarations
import Text.Megaparsec.CSS.Types

cssRuleSets :: CSSParser [RuleSet]
cssRuleSets = do
    void $ hspace
    rs <- some cssRuleSet
    void $ hspace
    return rs

cssDocRS :: CSSParser CSSDoc
cssDocRS = do
    rs <- cssRuleSets
    return (CSSDocRS rs)

cssDocContainer :: CSSParser CSSDoc
cssDocContainer = do
    cnt <- some cssContainer
    return (CSSDocC cnt)

cssDoc :: CSSParser CSSDoc
cssDoc = do
    void $ hspace
    doc <- (cssDocRS <|> cssDocContainer)
    void $ hspace
    return doc
