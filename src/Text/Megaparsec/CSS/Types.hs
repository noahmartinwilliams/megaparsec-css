module Text.Megaparsec.CSS.Types where

import Data.Int
import Data.Text
import Data.Void
import Text.Megaparsec

type Parser = Parsec Void Text

data ColorVal = ColorName Text | ColorHexVal Int8 Int8 Int8 Int8 deriving(Show, Eq)

data ColorType = BGColor | FGColor deriving(Show, Eq)

data Declaration = ColorDeclaration ColorType ColorVal | SizeDeclaration SizeType Size deriving(Show, Eq)

data Size = SizePx Int deriving(Show, Eq)

data SizeType = BorderRadius deriving(Show, Eq)
