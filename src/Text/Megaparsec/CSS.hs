module Text.Megaparsec.CSS(
module Text.Megaparsec.CSS.Types,
cssDoc) where

import Text.Megaparsec
import Text.Megaparsec.CSS.Declarations 
import Text.Megaparsec.CSS.Space as S
import Text.Megaparsec.CSS.Types

cssDoc :: Parser Doc
cssDoc = do
    m <- many (S.lexeme cssDeclarationBlock)
    return (Doc m)
