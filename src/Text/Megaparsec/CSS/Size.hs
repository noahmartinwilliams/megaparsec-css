{-# LANGUAGE FlexibleContexts #-}
module Text.Megaparsec.CSS.Size where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Text.Megaparsec.CSS.Types
import Text.Megaparsec.CSS.Space

cssSizeType :: Parser SizeType
cssSizeType = do
    stype <- (string (T.pack "border-radius"))
    if stype == (T.pack "border-radius")
    then
        return BorderRadius
    else
        undefined

cssSizeVal :: Parser Size
cssSizeVal = do
    nums <- many digitChar
    void $ lexeme (string (T.pack "px"))
    return (SizePx (read nums :: Int))
