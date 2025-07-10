{-# LANGUAGE FlexibleContexts #-}
module Text.Megaparsec.CSS.Properties where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad

cssProperty :: (MonadParsec Void s m, Token s ~ Char) => m [Token s]
cssProperty = ((return "background-color") <|> (return "foreground-color"))
