{-# LANGUAGE FlexibleContexts #-}
module Text.Megaparsec.CSS.Values where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad
import Text.Megaparsec.CSS.Types
import Text.Megaparsec.CSS.Colors

cssColorValNamed :: (Stream s , Token s ~ Char, Tokens s ~ String) => Parsec Void s String
cssColorValNamed = do
    let colors = fst (unzip allColors)
    choice (map string colors) where
