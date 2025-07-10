{-# LANGUAGE FlexibleContexts #-}
module Text.Megaparsec.CSS.Properties where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

cssColorProperty :: (Stream s , Token s ~ Char, Tokens s ~ String) => Parsec Void s String
cssColorProperty = ((string "background-color") <|> (string "foreground-color"))
