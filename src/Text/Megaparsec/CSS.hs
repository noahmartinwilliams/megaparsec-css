module Text.Megaparsec.CSS ( cssRuleSets, module Text.Megaparsec.CSS.Types) where

import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.CSS.Declarations
import Text.Megaparsec.CSS.Types

cssRuleSets :: CSSParser [RuleSet]
cssRuleSets = do
    void $ hspace
    rs <- some cssRuleSet
    void $ hspace
    return rs

cssDoc :: CSSParser CSSDoc
cssDoc = do
    rs <- cssRuleSets
    return (CSSDoc rs)
