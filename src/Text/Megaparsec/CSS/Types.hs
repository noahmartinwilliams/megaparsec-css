module Text.Megaparsec.CSS.Types where

import Data.Int

data ColorVal = ColorName String | ColorHexVal Int8 Int8 Int8 Int8 deriving(Show, Eq)

data ColorType = BGColor | FGColor deriving(Show, Eq)

data Declaration = ColorDeclaration ColorType ColorVal deriving(Show, Eq)
